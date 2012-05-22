#lang racket

; Fragments: an expansion-time monad for generating reagent code

(require "atomic-ref.rkt" "keywords.rkt"
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
	 reflect-fragment
	 close-fragment
	 reify-fragment)

;; for debugging only
(provide with-cas with-retry-handler with-block-handler retry block continue-with
         static-kcas! bind do-kcas!)

; the continuation environment
(define-syntax-parameter continue-with (syntax-rules ())) ; normal continuation
(define-syntax-parameter retry (syntax-rules ())) ; transient failure
(define-syntax-parameter block (syntax-rules ())) ; permanent failure

(define-syntax-parameter kcas-list '())

(define-simple-macro (with-cas (box ov nv) body ...)
  (syntax-parameterize ([kcas-list (cons (list #'box #'ov #'nv) 
					 (syntax-parameter-value #'kcas-list))])
    body ...))

(define-simple-macro (with-retry-handler handler body ...)
  (syntax-parameterize ([retry (syntax-parser [(retry) #'handler])])
    body ...))

(define-simple-macro (with-block-handler handler body ...)
  (syntax-parameterize ([block (syntax-parser [(block) #'handler])])
    body ...))

(define-syntax (cas!-fragment stx)
  (define/with-syntax (b ov nv) (generate-temporaries '(b ov nv)))
  (syntax-parse stx
    [(_ ar-e ov-e nv-e)
     #'(let ([b (unsafe-struct*-ref ar-e 0)]
	     [ov ov-e]
	     [nv nv-e])
	 (with-cas (b ov nv) (continue-with (void))))]))

(define-syntax (read-match-fragment stx)
  (define/with-syntax (b ov nv) (generate-temporaries '(b ov nv)))
  (define-syntax-class clause
    #:literals (update-to!)
    #:attributes (mclause)
    (pattern [pat (prelude ...) pre ... (update-to! up-e) post ...]
             #:with mclause
             #'[pat prelude ... (sequence pre ... 
                              (let ([nv up-e]) (with-cas (b ov nv) (continue-with (void))))
                              post ...)])
    (pattern [pat (prelude ...) (update-to!) post ...]
             #:with mclause
             #'[pat prelude ...
		    (sequence (with-cas (b ov ov) (continue-with (void)))
			      post ...)]))
  (syntax-parse stx #:literals (update-to!)
    [(_ ar-e cl:clause ...)
     #'(let* ([b (atomic-ref-box ar-e)]
              [ov (unsafe-unbox* b)])
         (match ov cl.mclause ...))]))

(define-simple-macro (bind (x:id e:expr) body ...)
  (syntax-parameterize ([continue-with 
			 (let ([old (syntax-parameter-value #'continue-with)])
                           (syntax-parser [(_ result)
                                           #`(let ([x result]) 
                                               (syntax-parameterize ([continue-with #,old]) body ...))]))])
     e))

(define-syntax (sequence stx)
  (syntax-parse stx
    [(_)   #'(continue-with (void))]
    [(_ f) #'f]
    [(_ f1 f ...)
     #'(bind (_ f1) (sequence f ...))]))

(define-syntax (choose-fragment stx)
  (define/with-syntax (alt alt-with-retry) (generate-temporaries '(alt alt-with-retry)))
  (syntax-parse stx
    [(_ f1 f2)
     #'(let* ([retry*         (λ () (retry))]
              [alt            (λ () f2)]              
              [alt-with-retry (λ () (with-block-handler (retry*) f2))])
	 (with-retry-handler (alt-with-retry)
          (with-block-handler (alt) f1)))]))

(define-simple-macro (reify-fragment f)
  (λ (k retry-k block-k)
    (syntax-parameterize
     ([continue-with (syntax-parser [(_ result) #`(k result #,(syntax-parameter-value #'kcas-list))])]
      [retry (syntax-parser [(_) #'(retry-k)])]
      [block (syntax-parser [(_) #'(block-k)])])
     f)))

(define-simple-macro (reflect-fragment runtime-fragment)
  (let ([k       (λ (result additional-kcas-list) ;; FIXME: accumulate dynamic KCAS clauses
		    (continue-with result))]
	[retry-k (λ () (retry))]
	[block-k (λ () (block))])
    (runtime-fragment k retry-k block-k)))

;; indirection so that this expands at the right time
(define-syntax (do-kcas! stx)
  (syntax-parse stx
    [(_) #`(static-kcas! #,@(syntax-parameter-value #'kcas-list))]))

(define-syntax (close-fragment stx)
  (define/with-syntax (retry-loop result) (generate-temporaries '(retry-loop result)))
  (syntax-parse stx
    [(_ f) #`(let retry-loop ()
	       (with-retry-handler (retry-loop)
		(with-block-handler (retry-loop)
		 (bind (result f) ;; FIXME: delay this allocation until after the kcas
		   (if (do-kcas!)
		       result
		       (retry))))))]))