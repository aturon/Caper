#lang racket

(require "syntax.rkt" "keywords.rkt"
         (for-syntax syntax/parse unstable/syntax racket/syntax racket/pretty
		     "syntax.rkt" "fragment.rkt")
	 (for-template racket/base))

(provide define-reagent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core reagent implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-macro (define-reagent (name:id arg:id ...) body:reagent-body)
  (define (name arg ...) (close-fragment body.fragment)))

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
     
(pmacro
 (define-reagent (r x)
   x))


#|

; eventually we'll store something like this for composition
(struct reagent (fragment)
  #:property prop:procedure
             (lambda (r) ... (reagent-fragment r)))

|#