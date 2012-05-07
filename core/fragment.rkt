#lang racket

; Fragments: an expansion-time monad for generating reagent code

; TODO: refactor this in an explicitly monadic style

(require (for-template racket/base "atomic-ref.rkt")
	 syntax/parse/define
	 racket/stxparam
         racket/syntax)
(provide pure-fragment 
	 cas!-fragment 
	 sequence-fragments 
	 choice-of-fragments 
	 read-match-fragment
	 expr-fragment
	 (struct-out read-match-clause-with-update)
	 (struct-out read-match-clause-no-update))

(struct read-match-clause-with-update (pat pre upd-exp post))
(struct read-match-clause-no-update (pat body))

(define retry-k (make-parameter (void)))
(define block-k (make-parameter (void)))
(define kcas-list (make-parameter (void)))

(define-simple-macro (with-kcas (box ov nv) body ...)
  (parameterize ([kcas-list (cons (list box ov nv) (kcas-list))])
    body ...))

(define ((pure-fragment val) k) (k val))

(define ((cas!-fragment atomic-ref-exp old-value-exp new-value-exp) k)
  (with-syntax ([(b ov nv) (generate-temporaries '(b ov nv))])
    #`(let ([b (atomic-ref-box #,atomic-ref-exp)]
	    [ov #,old-value-exp]
	    [nv #,new-value-exp])
	#,(with-kcas (#'b #'ov #'nv) (k (void))))))

(define ((read-match-fragment atomic-ref-exp clauses) k)
  (define/with-syntax (b ov nv) (generate-temporaries '(b ov nv)))
  (define (finish-clause clause)
    (match clause
      [(read-match-clause-with-update pat pre upd-exp post)
       (define (upd-fragment k)
	 (with-syntax ([finish (k retry-k block-k (void) (cons #'(b ov nv) kcas-list))])
	   #`(let ([nv #,upd-exp]) 
	       #,
	       finish)))
       #`[#,pat #,((sequence-fragments pre upd-fragment post) k retry-k block-k kcas-list)]]
      [(read-match-clause-no-update pat body)
       ; eventually, we probably want to mark "visible reads" differently in the kcas-list
       #`[#,pat #,(body k retry-k block-k (cons #'(b ov ov) kcas-list))]]))
  (with-syntax ([(finished-clause ...) (map finish-clause clauses)])
    #`(let ([b (atomic-ref-box #,atomic-ref-exp)]
	    [ov (unsafe-unbox* b)])
	(match ov finished-clause ...))))

(define ((bind f fk)
	 k retry-k block-k kcas-list)
  (f (lambda (retry-k block-k result kcas-list)
       ((fk result) k retry-k block-k kcas-list))
     retry-k block-k kcas-list))

(define (sequence-fragments . fs)
  (match fs
    [(list)      (pure-fragment (void))]
    [(list f)    f]
    [(cons f fs) (bind f (lambda (_) (apply sequence-fragments fs)))]))

(define ((choice-of-fragments f1 f2)
         k retry-k block-k kcas-list)
  (with-syntax* ([(alt alt-with-retry) (generate-temporaries '(alt alt-with-retry))]
                 [first-body           (f1 k #'(alt-with-retry) #'(alt) kcas-list)]
                 [alt-body             (f2 k retry-k block-k kcas-list)]
                 [alt-with-retry-body  (f2 k retry-k retry-k kcas-list)])
    #'(let ([alt            (lambda () alt-body)]
	    [alt-with-retry (lambda () alt-with-retry-body)])
	first-body)))

(define ((expr-fragment e)
	 k retry-k block-k kcas-list)
  (with-syntax ([result (generate-temporary 'result)])
    #`(let ([result #,e])
	#,(k retry-k block-k #'result kcas-list))))
  
; the final continuation
(define (commit retry-k block-k result kcas-list)
  #`(if (static-kcas! #,@kcas-list) #,result #,retry-k))

(define (close-fragment f)
  (with-syntax* ([retry (generate-temporary 'retry)]
                 [finish (f commit #'(retry) #'(retry) '())])
    #'(let retry () finish)))