#lang racket/base

(require "syntax.rkt" "keywords.rkt" "fragment.rkt"
	 syntax/parse/define
	 macro-debugger/expand racket/stxparam racket/match racket/pretty
         (for-syntax "syntax.rkt" racket/base syntax/parse unstable/syntax racket/syntax racket/pretty)
	 (for-template racket/base))

(provide define-reagent cas! read-match update-to!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core reagent implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-macro (define-reagent header:expr body:reagent-body)
  (define header (close-fragment body.fragment)))

(define (cleanup s)  
  (match s
    [(list 'syntax-parameterize b x) (cleanup x)]
    [(list 'syntax-parameterize b x ...) (cleanup `(let () ,@x))]
    [(list x ...) (map cleanup x)]
    [_ s]))

(define-syntax (pmacro stx)
  (syntax-parse stx
    [(_ e:expr) #'(begin (pretty-print (syntax->datum #'e))
			 (pretty-print
                          (cleanup
                           (syntax->datum
                            (expand-only 
                             #'e 
                             (list #'define-reagent #'sequence #'close-fragment #'cas!-fragment
                                   #'with-retry-handler #'with-block-handler #'bind #'with-cas
                                   #'choose-fragment #'read-match-fragment
                                   #'retry #'block #'continue-with #'static-kcas! #'do-kcas!))))))]))

(define b '())
(define o '())
(define n '())

(define-reagent (r x)
  (cas! b o n)
  (cas! b o n))


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