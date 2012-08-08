#lang racket

(require caper/core/top-level
         syntax/parse/define
         (for-syntax syntax/parse))

(define-simple-macro (define-reagent-macro (id pat ...) body)
  (define-reagent-syntax id
    (syntax-parser [(_ pat ...) #'body])))

(define-reagent-macro (bind x e)
  (bind-values (x) e))

(define-reagent-macro (match-reagent e clause ...)
  (begin-reagent
   (bind x e)
   (#%match x clause ...)))