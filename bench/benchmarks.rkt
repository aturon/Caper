#lang racket/base

(require "bench-tools.rkt" "../treiber-stack.rkt")
(provide bench-treiber-stack)

(define (bench-treiber-stack)
  (define s (make-treiber-stack))
  (measure-all-throughputs 
   (begin (push s 0)
	  (pop s))))