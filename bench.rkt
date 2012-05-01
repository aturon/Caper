#lang racket/base

(require (for-syntax racket/base syntax/parse) racket/future)

(provide measure-throughput)

;; measure-throughput: syntax -> syntax

;; (measure-throughput code threads) measures how many times on average `code'
;; executes, per millisecond, while run in `threads' parallel threads.
(define-syntax (measure-throughput stx)
  (syntax-parse stx
    [(_ code threads:integer)
     (with-syntax ([(tid ...) (generate-temporaries (build-list (syntax-e #'threads) (lambda (x) x)))]
                   ; total iterations is currently a magic constant, but should
                   ; eventually be dynamically determined to be large enough to
                   ; get an accurate measurement.
                   [iters #'100000000])
       #'(let ()
           (define iters/thread (floor (/ iters threads)))
           (define (thread-body)
             (for ([i (in-range iters/thread)]) code))
           (define start-time (current-milliseconds))
           (let ([tid (future thread-body)] ...)
             (touch tid) ...)
           (define end-time (current-milliseconds))
           (/ iters (- end-time start-time))))]))