#lang racket

; a specialized form of box usable within reagents; allows KCAS operations

(require "kcas.rkt"
         racket/future racket/unsafe/ops)
(provide atomic-ref
         atomic-ref?
         atomic-ref-read
         atomic-ref-cas!
         atomic-ref-box ;; internal library use only!
         unsafe-atomic-ref-box  ;; internal use only!
	 blocking-atomic-ref
	 blocking-atomic-ref?
	 blocking-atomic-ref-box
	 blocking-atomic-ref-waiters
	 waiter-group
	 waiter-group?
	 waiter-group-enroll
	 waiter-group-signal-all
)

; the type "atomic-ref" is a synonym for internal-atomic-ref
(struct internal-atomic-ref (box))

; atomic-ref: any/c -> atomic-ref?
; constructs an atomic reference holding some initial value
(define (atomic-ref init)
  (internal-atomic-ref (box init)))

(define-syntax-rule (atomic-ref-box aref)
  (internal-atomic-ref-box aref))

(define-syntax-rule (atomic-ref? aref)
  (internal-atomic-ref? aref))

(define-syntax-rule (unsafe-atomic-ref-box b)
  (unsafe-struct-ref b 0))

; atomic-ref-read: atomic-ref? -> any/c
; snapshots the current value of an atomic-ref
(define (atomic-ref-read ar)
  (read-box (atomic-ref-box ar)))

; atomic-ref-cas!: atomic-ref? any/c any/c -> boolean?
; (atomi-ref-cas! ar ov nv) attempts atomically update the atomic-ref ar from ov
; to nv, returning a boolean indicating success
(define (atomic-ref-cas! ar ov nv)
  (unsafe-box*-cas! (atomic-ref-box ar) ov nv))

(struct waiter-group (enroll signal-all))

; the type "blocking-atomic-ref" is a synonym for internal-blocking-atomic-ref
(struct internal-blocking-atomic-ref internal-atomic-ref (waiters))

; blocking-atomic-ref: any/c -> atomic-ref?
; constructs an atomic reference holding some initial value
(define (blocking-atomic-ref init waiters)
  (internal-blocking-atomic-ref (box init) waiters))

(define-syntax-rule (blocking-atomic-ref-box baref)
  (internal-blocking-atomic-ref-box baref))

(define-syntax-rule (blocking-atomic-ref? baref)
  (internal-blocking-atomic-ref? baref))

(define-syntax-rule (blocking-atomic-ref-waiters baref)
  (internal-blocking-atomic-ref-waiters baref))