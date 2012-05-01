#lang racket/base

(require "bench-tools.rkt" "../treiber-stack.rkt" racket/performance-hint)
(provide bench-treiber-stack)

(begin-encourage-inline
  (define (bench-treiber-stack)
    (define s (make-treiber-stack))
    (measure-all-throughputs 
     (begin (push* s 0)
            (pop* s)))))