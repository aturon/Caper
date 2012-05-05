#lang racket

(require syntax/parse unstable/syntax racket/syntax
         "atomic-ref.rkt"
         "syntax.rkt"
         "fragment.rkt"
         (for-syntax "atomic-ref.rkt"))

(provide define-reagent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core reagent implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (static-kcas! kcas-list)
  (syntax-parse (datum->syntax #'dummy kcas-list)
    [()
     #'#t]
    [((b ov nv))
     #'(unsafe-box*-cas! b ov nv)]
    [((b ov nv) ...)
     #'(kcas! (list (kcas-item b ov nv) ...))]))

; the final continuation
(define (commit retry-k block-k result kcas-list)
  (with-syntax ([try-kcas (static-kcas! kcas-list)])
    #`(if try-kcas #,result #,retry-k)))

(define (close-fragment f)
  (with-syntax* ([retry (generate-temporary 'retry)]
                 [finish (f commit #'(retry) #'(retry) '())])
    #'(let retry ()
        finish)))

(define (define-reagent stx)
  (syntax-parse stx 
    [(_ (name arg ...) body:reagent-body)
     (with-syntax ([closed-body (close-fragment (attribute body.fragment))])
       #'(define (name arg ...) closed-body))]))

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