#lang racket

(require syntax/parse unstable/syntax racket/syntax
         "atomic-ref.rkt"
         (for-syntax "atomic-ref.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keywords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; cribbed from class-internal.rkt
(define-for-syntax (do-keyword stx orig-sym)
  (let ([orig-stx (datum->syntax #f orig-sym stx)])
    (if (identifier? stx)
        (raise-syntax-error
         #f
         "illegal (unparenthesized) use of a define-reagent keyword"
         orig-stx)
        (raise-syntax-error
         #f
         "use of a define-reagent keyword is not in a define-reagent top-level"
         orig-stx))))

(define-syntax provide-keyword
  (syntax-rules ()
    [(_ id ...)
     (begin
       (define-syntax (id stx) (do-keyword stx 'id))
       ...
       (provide id ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide-keyword cas! choice)
(provide define-reagent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core reagent implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (empty-fragment k retry-k block-k kcas-list)
  (k retry-k block-k (void) kcas-list))

(define ((cas-fragment box-exp ov-exp nv-exp) k retry-k block-k kcas-list)
  (with-syntax ([(b ov nv) (generate-temporaries '(b ov ov))]
                [finish (k retry-k block-k (void) (cons #'(b ov nv) kcas-list))])
    #`(begin (define b  (atomic-ref-box #,box-exp))
	     (define ov #,ov-exp)
	     (define nv #,nv-exp)
	     finish)))

(define ((sequence-fragments f1 f2) k retry-k block-k kcas-list)
  (f1 (lambda (retry-k block-k result kcas-list)
        (f2 k retry-k block-k kcas-list))
      retry-k block-k kcas-list))

(define ((choice-of-fragments f1 f2) k retry-k block-k kcas-list)
  (with-syntax ([(alt alt-with-retry) (generate-temporaries '(alt alt-with-retry))]
                [first-body           (f1 k #'(alt-with-retry) #'(alt) kcas-list)]
		[alt-body             (f2 k retry-k block-k kcas-list)]
		[alt-with-retry-body  (f2 k retry-k retry-k kcas-list)])
    #'(begin (define (alt) alt-body)
	     (define (alt-with-retry) alt-with-retry-body)
	     first-body)))

(define (static-kcas! kcas-list)
  (syntax-parse (datum->syntax #'dummy kcas-list)
    [()
     #'#t]
    [((b ov nv))
     #'(unsafe-box*-cas! b ov nv)]
    [((b ov nv) ...)
     #'(kcas! (list (kcas-item b ov nv) ...))]))

(define (commit retry-k block-k result kcas-list)
  (with-syntax ([try-kcas (static-kcas! kcas-list)])
    #`(if try-kcas #,result #,retry-k)))

(define (interpret-clause clause)
  (syntax-parse clause #:literals (cas! choice)
    [(cas! box-exp ov-exp nv-exp)
     (cas-fragment #'box-exp #'ov-exp #'nv-exp)]
    [(choice r1 r2)
     (choice-of-fragments (interpret-clauses #'r1)
                          (interpret-clauses #'r2))]))

(define (interpret-clauses clauses)
  (foldr sequence-fragments
         empty-fragment
         (syntax-map interpret-clause clauses)))

(define (close-fragment f)
  (with-syntax ([retry (generate-temporary 'retry)]
                [finish (f commit #'(retry) #'(retry) '())])
    #'(let retry ()
        finish)))

(define (define-reagent stx)
  (syntax-parse stx 
    [(_ (name arg ...) . clauses)
     (with-syntax ([body (close-fragment (interpret-clauses #'clauses))])
       #'(define (name arg ...) body))]))

(pretty-print
 (syntax->datum (define-reagent
                  #'(define-reagent (r x)))))

(pretty-print
 (syntax->datum (define-reagent
                  #'(define-reagent (r x)
                      (cas! box old new)))))

(pretty-print
 (syntax->datum (define-reagent
                  #'(define-reagent (r x)
                      (cas! box old new)
                      (cas! box old new)))))

(pretty-print
 (syntax->datum (define-reagent
                  #'(define-reagent (r x)
                      (choice
                       ((cas! box old new))
                       ((cas! box old new)))))))

#|

; eventually we'll store something like this for composition
(struct reagent (fragment)
  #:property prop:procedure
             (lambda (r) ... (reagent-fragment r)))

|#