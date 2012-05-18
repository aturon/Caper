#lang racket

(require "syntax.rkt" "keywords.rkt" "fragment.rkt"
	 syntax/parse/define
	 macro-debugger/expand
         (for-syntax "syntax.rkt" syntax/parse unstable/syntax racket/syntax racket/pretty)
	 (for-template racket/base))

(provide define-reagent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core reagent implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-macro (define-reagent (name:id arg:id ...) body:reagent-body)
  (define (name arg ...) (close-fragment body.fragment)))

(define-syntax (pmacro stx)
  (syntax-parse stx
    [(_ e:expr) #'(begin (pretty-print (syntax->datum (expand-once #'e)))
			 (pretty-print
                          (syntax->datum
                           (expand-only 
                            #'e 
                            (list #'define-reagent #'sequence #'close-fragment #'cas!-fragment
                                  #'with-retry-handler #'with-block-handler #'bind #'with-cas
                                  #'retry #'block #'continue-with #'static-kcas! #'do-kcas!)))))]))

(define b '())
(define o '())
(define n '())

(pmacro (define-reagent (r x)
   (cas! b o n)))

#|
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
|#

#|

; eventually we'll store something like this for composition
(struct reagent (fragment)
  #:property prop:procedure
             (lambda (r) ... (reagent-fragment r)))

|#