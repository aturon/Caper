#lang racket/base

; provides generic benchmarking tools

(require (for-syntax racket/base syntax/parse) racket/future)
(provide measure-throughput measure-all-throughputs)

; total iterations is currently a magic constant, but should
; eventually be dynamically determined to be large enough to
; get an accurate measurement.
(define iters 100000000)

; (measure-throughput code threads) measures how many times on average `code'
; executes, per microsecond, while run in `threads' parallel threads.
(define-syntax (measure-throughput stx)
  (syntax-parse stx
    [(_ threads:integer code)
     (with-syntax ([(tid ...) (generate-temporaries (build-list (syntax-e #'threads) (λ _ 'tid)))])
       #'(let ()
           (define iters/thread (floor (/ iters threads)))
           (define (thread-body)
             (for ([i (in-range iters/thread)]) code))
           (define start-time (current-inexact-milliseconds))
           (let ([tid (future thread-body)] ...)
             (touch tid) ...)
           (define end-time (current-inexact-milliseconds))
           (/ (/ iters (- end-time start-time)) 1000)))]))

; temporary magic constant
(define-for-syntax max-threads 8)

(define-syntax (measure-all-throughputs stx)
  (syntax-parse stx
    [(_ code)
     (with-syntax ([(measurement ...)
		    (build-list (sub1 max-threads) 
				(lambda (n) #`(cons #,(add1 n) (measure-throughput #,(add1 n) code))))])
       #'(list measurement ...))]))