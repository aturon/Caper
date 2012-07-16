#lang racket/base

; generic tools for multithreaded testing

(require (for-syntax racket/base syntax/parse) racket/future)
(provide multithreaded-for-effect)

; number of total iterations for multithreaded testing
(define iters 1000000)

; (multithreaded-for-effect code threads) runs `code` within `threads' threads
; for a large number of iterations.
(define-syntax (multithreaded-for-effect stx)
  (syntax-parse stx
    [(_ threads:integer code)
     (with-syntax ([(tid ...) (generate-temporaries (build-list (syntax-e #'threads) (λ _ 'tid)))])
       #'(let ()
           (define iters/thread (floor (/ iters threads)))
           (define (thread-body)
             (for ([i (in-range iters/thread)]) code))
           (let ([tid (future thread-body)] ...)
             (touch tid) ...)))]))
