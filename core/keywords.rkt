#lang racket

; sets up bindings for keywords internal to the reagent forms

; cribbed from class-internal.rkt
(define-for-syntax (do-keyword stx orig-sym)
  (let ([orig-stx (datum->syntax #f orig-sym stx)])
    (if (identifier? stx)
        (raise-syntax-error
         #f
         "illegal (unparenthesized) use of a reagent keyword"
         orig-stx)
        (raise-syntax-error
         #f
         "use of a reagent keyword is not in an appropriate reagent context"
         orig-stx))))

(define-syntax provide-keyword
  (syntax-rules ()
    [(_ id ...)
     (begin
       (define-syntax (id stx) (do-keyword stx 'id))
       ...
       (provide id ...))]))

(provide provide-keyword)

(provide-keyword #%read #%cas! #%choose #%match
                 block retry prelude postlude begin-reagent computed bind-values)