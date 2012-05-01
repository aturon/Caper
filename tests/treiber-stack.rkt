#lang racket/base

(require "../treiber-stack.rkt" 
	 "test-tools.rkt"
	 rackunit)

(define s (make-treiber-stack))

(check-equal? (pop s #f) #f "pop on empty stack not failure-result")

(push s 1)
(check-equal? (pop s) 1 "pop on singleton stack not singleton")
(check-equal? (pop s #f) #f "pop on empty stack not failure-result")

(push s 1)
(check-equal? (pop s #f) 1 "pop on singleton stack not singleton")
(check-equal? (pop s #f) #f "pop on empty stack not failure-result")

(push s 1)
(push s 2)
(check-equal? (pop s) 2 "pop on two-element stack not top element")
(check-equal? (pop s) 1 "second pop on two-element stack not bottom element")
(check-equal? (pop s #f) #f "pop on empty stack not failure-result")

(multithreaded-for-effect 2
  (begin (push s 1)
	 (pop s)))
(check-equal? (pop s #f) #f "simple multithreaded push/pop test failed")