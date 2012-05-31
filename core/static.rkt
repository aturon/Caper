#lang racket/base

(require (for-template racket/base "fragment.rkt"))
(provide (struct-out reagent))

(require syntax/parse)
(struct reagent (formals prelude fragment)
  #:property prop:procedure
  (lambda (r stx) 
    (syntax-parse stx
      [(f actuals ...) 
       #`(#,(datum->syntax stx '#%app) f actuals ...)]
      [_ #`(lambda #,(reagent-formals r)
             #,@(reagent-prelude r)
             (reify-fragment #,(reagent-fragment r)))])))