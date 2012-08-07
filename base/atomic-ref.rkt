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




(define-syntax-class read-match-clause
  ;; can't lift preludes above here, because they might
  ;; escape their binders
  #:literals (update-to!)
  (pattern (match-pat pre:reagent-clause ...
                      (update-to! new-value:expr)
                      post:reagent-clause ...)
           #:attr fragment
	          #'(match-pat [pre.prelude ... ...
				post.prelude ... ...]
			       pre.fragment ...
			       (update-to! new-value)
			       post.fragment ...))
   (pattern (match-pat f:reagent-clause ...)
            #:attr fragment 
                   #'(match-pat [f.prelude ... ...]
				(update-to!) 
				f.fragment ...)))




(define-syntax (read-match-fragment stx)
  (define/with-syntax (ar bx ov nv) (generate-temporaries '(ar bx ov nv)))
  (define-syntax-class clause
    #:literals (update-to!)
    #:attributes (mclause)
    (pattern [pat (prelude ...) pre ... (update-to! up-e) post ...]
             #:with mclause
             #'[pat prelude ... 
                    (sequence pre ... (cas!-fragment ar bx ov up-e) post ...)])
    (pattern [pat (prelude ...) (update-to!) post ...]
             #:with mclause
             #'[pat prelude ...
		    (sequence (with-cas (bx ov ov) (continue-with (void))) post ...)]))
  (syntax-parse stx #:literals (update-to!)
    [(_ aref-e bx-e cl:clause ...)
     #'(let* ([ar aref-e]
	      [bx bx-e]
	      [ov (unsafe-unbox* bx)])
	 (when-offer (offer)
           (when (blocking-atomic-ref? ar)
	     ((waiter-group-enroll (blocking-atomic-ref-waiters ar)) offer)))
         (match ov 
	   cl.mclause ... 
	   [_ (block)]))]))


(define-simple-macro (cas!-fragment aref b ov-e nv-e)
  (let ([ov ov-e]
	[nv nv-e])
    (with-cas (b ov nv) 
      (with-postlude (when (blocking-atomic-ref? aref)
		       ((waiter-group-signal-all (blocking-atomic-ref-waiters aref))))
        (continue-with (void))))))

#|

  (pattern (cas! atomic-ref:expr old-value:expr new-value:expr)
           #:with (bv bx) (generate-temporaries '(bv bx))
           #:attr fragment #'(cas!-fragment bv bx old-value new-value)
           #:with (prelude ...) 
	   #'((define bv atomic-ref)
	      (unless (atomic-ref? bv)
		(error 'cas! "atomic-ref expected, but got" bv))
              (define bx (atomic-ref-box bv))))

  (pattern (read-match atomic-ref:expr clause:read-match-clause ...)
           #:with (bv bx) (generate-temporaries '(bv bx))
           #:attr fragment #'(read-match-fragment bv bx clause.fragment ...)
           #:with (prelude ...)
	   #'((define bv atomic-ref)
	      (unless (atomic-ref? bv)
		(error 'read-match "atomic-ref expected, but got" bv))
              (define bx (atomic-ref-box bv))))

|#