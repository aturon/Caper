#lang racket

(require caper/core/semantics (for-syntax caper/core/syntax) syntax/parse/define)
(provide (all-defined-out))

(define-simple-macro (define-reagent (name:id arg ...) body:reagent-body)
  (define-syntax name (static-reagent (list #'arg ...) #'(body.prelude ...) #'body.payload)))

; FIXME: should the delimited code be shared between different react invocations?
(define-simple-macro (react body:reagent-body)
  (begin body.prelude ... (#%delimit body.payload))) 

; FIXME: should this include arguments, more like a lambda form?
(define-simple-macro (reagent body ...)
  (let () (define-reagent (r) body ...) (r)))

(define-simple-macro (define-reagent-syntax id rhs)
  (define-syntax id (reagent-macro rhs)))