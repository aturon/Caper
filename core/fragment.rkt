#lang racket

; Fragments: an expansion-time monad for generating reagent code

(require caper/core/atomic-ref
	 caper/core/kcas
	 caper/core/keywords
	 caper/core/thread
	 (for-syntax syntax/parse 
		     racket/syntax)
         racket/unsafe/ops
	 syntax/parse/define
	 racket/stxparam
	 racket/stxparam-exptime)
(provide continue-with
         cas!-fragment 
	 sequence
	 choose-fragment
	 read-match-fragment
	 with-postlude
	 reflect-fragment
	 close-fragment
	 reify-fragment)

;; for debugging only
(provide with-cas with-retry-handler with-block-handler retry block continue-with with-offer
         static-kcas! bind do-kcas!)

; the continuation environment
(define-syntax-parameter continue-with (syntax-rules ())) ; normal continuation
(define-syntax-parameter retry (syntax-rules ())) ; transient failure
(define-syntax-parameter block (syntax-rules ())) ; permanent failure

(define-syntax-parameter current-offer #f) ; syntax for the offer, or #f if there is none
(define-syntax-parameter kcas-list '()) ; CAS clauses to perform
(define-syntax-parameter postlude-action #'(void)) ; postlude action to perform

(define-simple-macro (with-cas (box ov nv) body ...)
  (syntax-parameterize ([kcas-list (cons (list #'box #'ov #'nv) 
					 (syntax-parameter-value #'kcas-list))])
    body ...))

(define-simple-macro (with-dyn-kcas dyn body ...)
  (syntax-parameterize ([kcas-list (cons #'dyn (syntax-parameter-value #'kcas-list))])
    body ...))

(define-simple-macro (with-postlude post body ...)
  (syntax-parameterize ([postlude-action (begin (syntax-parameter-value #'postlude-action) #'post)])
    body ...))

(define-simple-macro (with-retry-handler handler body ...)
  (syntax-parameterize ([retry (syntax-parser [(retry) #'handler])])
    body ...))

(define-simple-macro (with-block-handler handler body ...)
  (syntax-parameterize ([block (syntax-parser [(block) #'handler])])
    body ...))

(define-simple-macro (with-offer offer body ...)
  (syntax-parameterize ([current-offer #'offer])
    body ...))

(define-simple-macro (cas!-fragment aref b ov-e nv-e)
  (let ([ov ov-e]
	[nv nv-e])
    (with-cas (b ov nv) 
      (with-postlude (when (blocking-atomic-ref? aref)
		       ((waiter-group-signal-all (blocking-atomic-ref-waiters aref))))
        (continue-with (void))))))

(define-syntax (if-offer stx)
  (define offer (syntax-parameter-value #'current-offer))
  (syntax-parse stx
   [(_ offer-formal true-body false-body)
    (if offer 
	#`(let ([offer-formal #,offer]) true-body)
	#'false-body)]))

(define-simple-macro (when-offer (offer-formal) body ...)
  (if-offer offer-formal (let () body ... (void)) (void)))

(define-syntax (read-match-fragment stx)
  (define/with-syntax (ar bx ov nv) (generate-temporaries '(ar bx ov nv)))
  (define-syntax-class clause
    #:literals (update-to!)
    #:attributes (mclause)
    (pattern [pat (prelude ...) pre ... (update-to! up-e) post ...]
             #:with mclause
             #'[pat prelude ... 
                    (sequence pre ... (cas!-fragment ar bx ov up-e) post ...)])
    (pattern [pat (prelude ...) (update-to!) post ...]
             #:with mclause
             #'[pat prelude ...
		    (sequence (with-cas (bx ov ov) (continue-with (void))) post ...)]))
  (syntax-parse stx #:literals (update-to!)
    [(_ aref-e bx-e cl:clause ...)
     #'(let* ([ar aref-e]
	      [bx bx-e]
	      [ov (unsafe-unbox* bx)])
	 (when-offer (offer)
           (when (blocking-atomic-ref? ar)
	     ((waiter-group-enroll (blocking-atomic-ref-waiters ar)) offer)))
         (match ov 
	   cl.mclause ... 
	   [_ (block)]))]))

(define-simple-macro (bind (x:id e:expr) body ...)
  (syntax-parameterize ([continue-with 
			 (let ([old (syntax-parameter-value #'continue-with)])
                           (syntax-parser [(_ result)
                                           #`(let ([x result]) 
                                               (syntax-parameterize ([continue-with #,old]) body ...))]))])
     e))

(define-syntax (sequence stx)
  (syntax-parse stx
    [(_)          #'(continue-with (void))]
    [(_ f)        #'f]
    [(_ f1 f ...) #'
     (syntax-parameterize ([continue-with 
			    (let ([old (syntax-parameter-value #'continue-with)])
			      (syntax-parser [(_ code)
					      #`(let () code
						     (syntax-parameterize ([continue-with #,old]) (sequence f ...)))]))])
			  f1)]))

(define-simple-macro (choose-fragment f1 f2)
  (let* ([retry*         (λ () (retry))]
	 [alt            (λ () f2)]              
	 [alt-with-retry (λ () (with-block-handler (retry*) f2))])
    (with-retry-handler (alt-with-retry)
     (with-block-handler (alt) f1))))

(define-simple-macro (reflect-environment k retry-k block-k body)
  (syntax-parameterize
     ([continue-with (syntax-parser [(_ result) 
				     #`(k result 
					  (flatten-mixed-kcas #,@(syntax-parameter-value #'kcas-list))
					  (lambda () #,(syntax-parameter-value #'postlude-action)))])]
      [retry (syntax-parser [(_) #'(retry-k)])]
      [block (syntax-parser [(_) #'(block-k)])])
    body))

(define-simple-macro (reify-fragment prelude f)
  (cons (λ (k retry-k block-k)
	   prelude
	   (reflect-environment k retry-k block-k f))
	(λ (k retry-k block-k offer)
	   prelude
	   (reflect-environment k retry-k block-k 
				(with-offer offer f)))))

(define-simple-macro (reflect-fragment runtime-fragment)
  (let ([k       (λ (result additional-kcas-list additional-postlude) 
		    (with-dyn-kcas additional-kcas-list 
		     (with-postlude (additional-postlude)
		      (continue-with result))))]
	[retry-k (λ () (retry))]
	[block-k (λ () (block))])
    (if-offer offer ((cdr runtime-fragment) k retry-k block-k offer)
	      ((car runtime-fragment) k retry-k block-k))))

;; indirection so that this expands at the right time
(define-syntax (do-kcas! stx)
  (syntax-parse stx
    [(_) #`(static-kcas! #,@(syntax-parameter-value #'kcas-list))]))

(define-syntax (do-postlude! stx)
  (syntax-parse stx
    [(_) (syntax-parameter-value #'postlude-action)]))

(define-simple-macro (close-fragment f)
  (let ()
    (define (try-with-offer)
      (define offer tl-semaphore) ; for now, just a semaphore
      (with-retry-handler (try-with-offer)
       (with-block-handler (begin (fsemaphore-wait offer)
				  (try-with-offer))
        (with-offer offer
         (bind (result f) ;; FIXME: delay this allocation until after the kcas
               (if (do-kcas!)
                   (begin (do-postlude!) result)
                   (retry)))))))
    (define (try-without-offer)
      (with-retry-handler (try-without-offer)
       (with-block-handler (try-with-offer)
	(bind (result f) ;; FIXME: delay this allocation until after the kcas
	      (if (do-kcas!)
		  (begin (do-postlude!) result)
		  (retry))))))
    (try-without-offer)))