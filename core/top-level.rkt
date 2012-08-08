#lang racket

(require syntax/parse/define
         caper/core/keywords
         caper/core/syntax
         (prefix-in sem: caper/core/semantics)
         (for-syntax caper/core/syntax syntax/parse))
(provide #%read #%cas! #%choose #%match
         prelude postlude begin-reagent computed bind-values
         define-reagent define-reagent-syntax react reagent)

(define-simple-macro (define-reagent (name:id arg ...) body:reagent-body)
  (define-syntax name (static-reagent (list #'arg ...) #'(body.prelude ...) #'body.payload)))

; FIXME: should the delimited code be shared between different react invocations?
(define-simple-macro (react body:reagent-body)
  (begin body.prelude ... (sem:#%delimit body.payload))) 

; FIXME: should this include arguments, more like a lambda form?
(define-simple-macro (reagent body ...)
  (let () (define-reagent (r) body ...) (r)))

(define-syntax define-reagent-syntax
  (syntax-parser
   [(_ id rhs)
    #'(define-syntax id (reagent-macro rhs))]
   [(_ (id arg ...) rhs)
    #'(define-syntax id (reagent-macro (lambda (arg ...) rhs)))]))

; Below: debugging infrastructure, needs to be reinstated

#|

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

|#