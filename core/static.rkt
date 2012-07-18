#lang racket/base

(require (for-template racket/base "fragment.rkt"))
(provide (struct-out reagent))

(require syntax/parse)
(struct reagent (formals prelude fragment)
  #:property prop:procedure
  ;; this is what happens when a statically-bound reagent is referenced *outside* of 
  ;; any other reagent computation -- ie, as a function
  (lambda (r stx) 
    (syntax-parse stx
      ;; redirect to the second case
      [(f actuals ...) 
       #`(#,(datum->syntax stx '#%app) f actuals ...)]
      [_ #`(λ #,(reagent-formals r)             
             (reify-fragment (begin #,@(reagent-prelude r)) #,(reagent-fragment r)))])))