;; Test dynamic reagent facilities

#lang racket/base

(require "../../core/atomic-ref.rkt" 
	 "../../core/reagent.rkt" 
	 "../test-tools.rkt"
	 rackunit)

(define ar (atomic-ref 0))

(define-reagent (cas-test old new)
  (cas! ar old new))

(react (cas-test 0 1))

(check-equal? (atomic-ref-read ar) 1 "static cas failed")

(define dyn-test cas-test)

(react (dynamic (dyn-test 1 2)))

(check-equal? (atomic-ref-read ar) 2 "dynamic cas failed")