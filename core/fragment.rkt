#lang racket

; Fragments: an expansion-time monad for generating reagent code

; TODO: refactor this in an explicitly monadic style

(require "atomic-ref.rkt"
	 (for-syntax syntax/parse 
		     racket/syntax)
	 syntax/parse/define
	 racket/stxparam
	 racket/stxparam-exptime)
(provide continue-with
         cas!-fragment 
	 sequence
	 choose-fragment
	 read-match-fragment
	 close-fragment)

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

;; (define-simple-macro (close-fragment e)
;; (syntax-parameterize ([continue-with 
;; 		       (syntax-parser 
;; 			[(_ a) 				       
;; 			 #`(if (static-kcas! #,@(syntax-parameter-value #'kcas-list))
;; 			       a 
;; 			       (retry))])])
;;    (let retry ()
;;      (with-retry-handler 
;;       (with-block-handler e retry)
;;       retry))))

;; (define-syntax (let-fresh stx)
;;   (syntax-parse stx
;;     [(let-fresh ([x:id e:expr] ...) body ...)    

(define-syntax (cas!-fragment stx)
  (define/with-syntax (b ov nv) (generate-temporaries '(b ov nv)))
  (syntax-parse stx
    [(_ ar-e ov-e nv-e)
     #'(let ([b (atomic-ref-box ar-e)]
	     [ov ov-e]
	     [nv nv-e])
	 (with-cas (b ov nv) (continue-with (void))))]))

(define-syntax (read-match-fragment stx)
  (define/with-syntax (b ov nv) (generate-temporaries '(b ov nv)))
  (syntax-parse stx #:literals (update-to!)
    [(_ ar-e [pat pre ... (update-to! up-e) post ...] ...)
     #'(let ([b (atomic-ref-box #,atomic-ref-exp)]
	     [ov (unsafe-unbox* b)])
	 (match ov [pat (sequence pre ... 
				  (let ([nv up-e]) (continue-with (void)))
				  post ...)] ...))]))

(define-simple-macro (bind (x:id e:expr) body ...)
  (syntax-parameterize ([continue-with 
			 (syntax-parser [(continue-with result)
					 #'(let ([x result]) body ...)])])
     e))

(define-syntax (sequence stx)
  (syntax-parse stx
    [(_)   #'(continue-with (void))]
    [(_ f) #'f]
    [(_ f1 f ...)
     #'(syntax-parameterize ([continue-with 
			      (syntax-parser [(continue-with result) (sequence f ...)])])
         f1)]))

(define-syntax (choose-fragment stx)
  (define/with-syntax (alt alt-with-retry) (generate-temporaries '(alt alt-with-retry)))
  (syntax-parse stx
    [(_ f1 f2)
     #'(let ([alt            (lambda () f2)]
	     [alt-with-retry (lambda () (with-block-handler (retry) f1))])  ; WARNING: retry is currently evaluated at the wrong time!
	 (with-retry-handler (alt-with-retry)
          (with-block-handler (alt) f1)))]))

(define-syntax (close-fragment stx)
  (define/with-syntax (retry-loop result) (generate-temporaries '(retry-loop result)))
  (syntax-parse stx
    [(_ f) #`(let retry-loop ()
	       (with-retry-handler (retry-loop)
		(with-block-handler (retry-loop)
		 (bind (result f)
		   (if (static-kcas! #,@(syntax-parameter-value #'kcas-list))
		       result
		       (retry))))))]))