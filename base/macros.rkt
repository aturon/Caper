#lang racket

(require caper/core/top-level
         syntax/parse/define
         (for-syntax syntax/parse))
(provide (all-defined-out))

(define-simple-macro (define-reagent-rewrite (id pat ...) body)
  (define-reagent-syntax id
    (syntax-parser [(_ pat ...) #'body])))

(define-reagent-rewrite (bind x e)
  (bind-values (x) e))

(define-reagent-rewrite (match-reagent e clause ...)
  (begin-reagent
   (bind x (values e))
   (#%match x clause ...)))

(define-reagent-syntax choice
  (syntax-parser
   [(_)         #'(block)]
   [(_ r)       #'r]
   [(_ r s ...) #'(#%choose r (choice s ...))]))

; a useful synonym for use with sequencing (rather than binding)
(define-reagent-rewrite (for-effect e ...)
  (values e ...))

