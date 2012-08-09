;; Test dynamic reagent facilities

#lang racket/base

(require caper/base/top-level
         caper/tests/test-tools
	 rackunit)

(define ar (atomic-ref 0))

(define-reagent (cas-test old new)
  (cas! ar old new))

(react (cas-test 0 1))

(check-equal? (atomic-ref-read ar) 1 "static cas failed")

(define dyn-test cas-test)

(react (computed (dyn-test 1 2)))

(check-equal? (atomic-ref-read ar) 2 "dynamic cas failed")