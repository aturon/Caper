#lang racket

; The semantics of the core reagent forms

(require caper/core/kcas
	 (for-syntax syntax/parse 
		     racket/syntax)
         racket/unsafe/ops
	 syntax/parse
	 syntax/parse/define
	 racket/syntax
	 racket/stxparam
	 racket/stxparam-exptime)
(provide #%return #%bind #%retry #%block #%cas! #%choose #%read #%postlude #%reflect #%reify #%delimit)

; TODO: replace this with an exported debugging expansion function
;; for debugging only
;; (provide with-cas 
;; 	 with-retry-handler 
;; 	 with-block-handler
;; 	 with-offer
;;          static-kcas! 
;; 	 do-kcas!)

; the continuation environment
(define-syntax-parameter #%return (syntax-rules ())) ; normal continuation
(define-syntax-parameter #%retry  (syntax-rules ())) ; transient failure
(define-syntax-parameter #%block  (syntax-rules ())) ; permanent failure

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
  (syntax-parameterize ([#%retry (syntax-parser [(#%retry) #'handler])])
    body ...))

(define-simple-macro (with-block-handler handler body ...)
  (syntax-parameterize ([#%block (syntax-parser [(#%block) #'handler])])
    body ...))

(define-simple-macro (with-offer offer body ...)
  (syntax-parameterize ([current-offer #'offer])
    body ...))

(define-simple-macro (#%bind e (x:id ...) k)
  (syntax-parameterize 
   ([#%return 
     (with-syntax ([old (syntax-parameter-value #'#%return)])
       (syntax-parser 
	[(_ result (... ...))
	 (syntax-parameterize ([#%return old])
           (let-values ([(x ...) (values result (... ...))])
	     k))]))])
   e))

(define-simple-macro (#%postlude e)
  (with-postlude e (#%return (void))))

(define-simple-macro (#%cas! b o n)
  (with-cas (b o n) (#%return (void))))

(define-simple-macro (#%read b)
  (let ([x (unbox b)])
    (with-cas (b x x)     ; TODO: add separate 'read' list
      (#%return x))))

(define-syntax (if-offer stx)
  (define offer (syntax-parameter-value #'current-offer))
  (syntax-parse stx
   [(_ offer-formal true-body false-body)
    (if offer 
	#`(let ([offer-formal #,offer]) true-body)
	#'false-body)]))

(define-simple-macro (when-offer (offer-formal) body ...)
  (if-offer offer-formal (let () body ... (void)) (void)))

(define-simple-macro (#%choose f1 f2)
  (let* ([retry*         (λ () (#%retry))]
	 [alt            (λ () f2)]              
	 [alt-with-retry (λ () (with-block-handler (retry*) f2))])
    (with-retry-handler (alt-with-retry)
     (with-block-handler (alt) f1))))

(define-simple-macro (reflect-environment k retry-k block-k body)
  (syntax-parameterize
     ([#%return (syntax-parser [(_ result) 
				     #`(k result 
					  (flatten-mixed-kcas #,@(syntax-parameter-value #'kcas-list))
					  (lambda () #,(syntax-parameter-value #'postlude-action)))])]
      [#%retry (syntax-parser [(_) #'(retry-k)])]
      [#%block (syntax-parser [(_) #'(block-k)])])
    body))

(define-simple-macro (#%reify prelude f)
  (cons (λ (k retry-k block-k)
	   prelude
	   (reflect-environment k retry-k block-k f))
	(λ (k retry-k block-k offer)
	   prelude
	   (reflect-environment k retry-k block-k 
				(with-offer offer f)))))

(define-simple-macro (#%reflect runtime-fragment)
  (let ([k       (λ (result additional-kcas-list additional-postlude) 
		    (with-dyn-kcas additional-kcas-list 
		     (with-postlude (additional-postlude)
		      (#%return result))))]
	[retry-k (λ () (#%retry))]
	[block-k (λ () (#%block))])
    (if-offer offer ((cdr runtime-fragment) k retry-k block-k offer)
	      ((car runtime-fragment) k retry-k block-k))))

(define (final-k stx)
  (define/with-syntax (cas-item ...) (syntax-parameter-value #'kcas-list))
  (define/with-syntax post-action    (syntax-parameter-value #'postlude-action))
  (syntax-parse stx
    [(_ result ...)
     #'(if (static-kcas! cas-item ...)
	   (begin (post-action) (values result ...))
	   (#%retry))])) 

(define-simple-macro (#%delimit f)
  (let ()
    (define (try-with-offer)
      (define offer tl-semaphore) ; for now, just a semaphore
      (with-retry-handler (try-with-offer)
       (with-block-handler (begin (fsemaphore-wait offer)
				  (try-with-offer))
        (with-offer offer
	 (syntax-parameterize ([#%return final-k])
	   f)))))
    (define (try-without-offer)
      (with-retry-handler (try-without-offer)
       (with-block-handler (try-with-offer)
	(syntax-parameterize ([#%return final-k])
	  f))))
    (try-without-offer)))