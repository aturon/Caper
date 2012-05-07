#lang racket

(require "syntax.rkt" "keywords.rkt" "atomic-ref.rkt"
         (for-syntax syntax/parse unstable/syntax racket/syntax racket/pretty
		     "syntax.rkt" "fragment.rkt" "atomic-ref.rkt")
	 (for-template racket/base racket/future racket/unsafe/ops
		       "atomic-ref.rkt"))

(provide define-reagent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core reagent implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; the final continuation
(define-for-syntax (commit retry-k block-k result kcas-list)
  #`(if (static-kcas! #,@kcas-list) #,result #,retry-k))

(define-for-syntax (close-fragment f)
  (with-syntax* ([retry (generate-temporary 'retry)]
                 [finish (f commit #'(retry) #'(retry) '())])
    #'(let retry () finish)))

(define-syntax (define-reagent stx)
  (syntax-parse stx 
    [(_ (name:id arg:id ...) body:reagent-body)
     (with-syntax ([closed-body (close-fragment (attribute body.fragment))])
       #'(define (name arg ...) closed-body))]))

(define-syntax (pmacro stx)
  (syntax-parse stx
    [(_ e:expr) #'(pretty-print (syntax->datum (expand-once #'e)))]))

(pmacro
 (define-reagent (r x)))

(pmacro
 (define-reagent (r x)
   (cas! box old new)))

(pmacro
 (define-reagent (r x)
   (cas! box old new)
   (cas! box old new)))

(pmacro
 (define-reagent (r x)
   (choice
    ((cas! box old new))
    ((cas! box old new)))))

(pmacro
 (define-reagent (r x)
   (choice
    ((cas! box old new)
     (cas! box old new))
    ((cas! box old new)))))

(pmacro
 (define-reagent (push s x)
   (read-match s
     [xs (update-to! (cons x xs))])))

(pmacro
 (define-reagent (pop s)
   (read-match s
     [(cons x xs) (update-to! xs) x]
     [_ #f])))
     

#|

; eventually we'll store something like this for composition
(struct reagent (fragment)
  #:property prop:procedure
             (lambda (r) ... (reagent-fragment r)))

|#