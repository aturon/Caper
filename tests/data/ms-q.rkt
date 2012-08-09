#lang racket/base

(require caper/data/ms-q
	 caper/tests/test-tools
	 rackunit)

(define q (make-queue))

(check-equal? (deq! q #f) #f "deq! on empty queue not failure-result")

(enq! q 1)
(check-equal? (deq! q) 1 "deq! on singleton queue not singleton")
(check-equal? (deq! q #f) #f "deq! on empty queue not failure-result")

(enq! q 1)
(check-equal? (deq! q #f) 1 "deq! on singleton queue not singleton")
(check-equal? (deq! q #f) #f "deq! on empty queue not failure-result")

(enq! q 1)
(enq! q 2)
(check-equal? (deq! q) 1 "deq! on two-element queue not top element")
(check-equal? (deq! q) 2 "second deq! on two-element queue not bottom element")
(check-equal? (deq! q #f) #f "deq! on empty queue not failure-result")

(multithreaded-for-effect 2
  (begin (enq! q 1)
	 (deq! q)))
(check-equal? (deq! q #f) #f "simple multithreaded enq!/deq! test failed")
