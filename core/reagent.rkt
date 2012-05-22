#lang racket/base

(require "syntax.rkt" "keywords.rkt" "fragment.rkt"
	 syntax/parse/define
	 macro-debugger/expand racket/stxparam racket/match racket/pretty
         (for-syntax "syntax.rkt" racket/base syntax/parse unstable/syntax racket/syntax racket/pretty)
	 (for-template racket/base))

(provide define-reagent cas! read-match update-to! react pmacro before)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core reagent implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-syntax
 (struct reagent (formals prelude fragment)
	 #:property prop:procedure
	 (lambda (r stx) 
	   (syntax-parse stx
	      [(f actuals ...) 
	       #`(#,(datum->syntax stx '#%app) f actuals ...)]
	      [_ #`(lambda #,(reagent-formals r)
                     #,@(reagent-prelude r)
		     (reify-fragment #,(reagent-fragment r)))]))))

(define-simple-macro (define-reagent (name:id arg ...) body:reagent-body)
  (define-syntax name (reagent (list #'arg ...) #'(body.prelude ...) #'body.fragment)))

(define-syntax (react stx)
  (syntax-parse stx
    [(_ (name:id actual ...))
     (define r (syntax-local-value #'name (λ () #f)))
     (cond [(reagent? r) 
            #`((λ #,(reagent-formals r)
                 #,@(reagent-prelude r)
                 (close-fragment #,(reagent-fragment r)))
               actual ...)]
	   [else
	    ;; FIXME: this introduces an extra eta-expansion; is that a problem?
	    #'(close-fragment (reflect-fragment (name actual ...)))])]))

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
                                   #'choose-fragment #'read-match-fragment #'react
                                   #'retry #'block #'continue-with #'static-kcas! #'do-kcas!
				   #'reflect-fragment #'reify-fragment))))))]))

(pmacro
 (begin
   (define-reagent (push s x)
     (read-match s
       [xs (update-to! (cons x xs))]))
   (react (push some-stack some-arg))
   (define push-alt push)
   (react (push-alt some-stack some-arg))))


#|
(define b '())
(define o '())
(define n '())

(define-reagent (r x)
  (cas! b o n)
  (cas! b o n))

(pmacro
 (begin
   (define-reagent (r x))
   (react (r z))))

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
 (begin
   (define-reagent (push s x)
     (read-match s
       [xs (update-to! (cons x xs))]))
   (react (push some-stack some-arg))))

(pmacro
 (define-reagent (pop s)
   (read-match s
     [(cons x xs) (update-to! xs) x]
     [_ #f])))
     
(pmacro
 (define-reagent (r x)
   x))

|#

(define-reagent (push2 s x)
  (read-match (tstack-head s)
    [xs (update-to! (cons x xs))]))
(pmacro (react (push2 s x)))
