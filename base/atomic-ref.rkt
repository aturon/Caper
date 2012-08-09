#lang racket

; a specialized form of box usable within reagents; allows KCAS operations

(require caper/core/kcas
         caper/core/top-level
         caper/core/keywords
         caper/base/macros
         racket/future
         racket/unsafe/ops
         (for-syntax syntax/parse racket/syntax))
(provide atomic-ref
         atomic-ref?
         atomic-ref-read
         atomic-ref-cas!
	 blocking-atomic-ref
	 blocking-atomic-ref?
	 waiter-group
	 waiter-group?
	 waiter-group-enroll
	 waiter-group-signal-all
         read
         read-match
         cas!
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


(define-reagent-rewrite (read aref)
  (begin-reagent
   (bind b (values (atomic-ref-box aref)))
   (read b)))

(define-reagent-rewrite (cas! aref-e ov-e nv-e)
  (begin-reagent
   (bind aref (values aref-e))
   (bind-values (b ov nv)
                (values (atomic-ref-box aref) ov-e nv-e))
   (#%cas! b ov nv)
   (postlude
    (when (blocking-atomic-ref? aref)
      ((waiter-group-signal-all (blocking-atomic-ref-waiters aref)))))))

(provide-keyword update-to!)

(define-reagent-syntax (read-match stx)
  (define/with-syntax (ar bx ov nv) (generate-temporaries '(ar bx ov nv)))
  (define-syntax-class match-clause
    #:literals (update-to!)
    
    (pattern [match-pat pre ... (update-to! nv-e:expr) post ...]
             #:attr transformed
             #'[match-pat pre ...
                          (bind nv (values nv-e))
                          (#%cas! bx ov nv)
                          post ...])
    
   (pattern [match-pat c ...]
            #:attr transformed
            #'[match-pat (#%cas! bx ov ov) c ...]))

  (syntax-parse stx
    [(_ aref-e cl:match-clause ...)
     #'(begin-reagent
        (bind ar (values aref-e))
        (bind bx (values (atomic-ref-box ar)))
        (bind ov (values (unsafe-unbox* bx)))
	 ;; (when-offer (offer)
         ;;   (when (blocking-atomic-ref? ar)
	 ;;     ((waiter-group-enroll (blocking-atomic-ref-waiters ar)) offer)))
        (match-reagent ov cl.transformed ... ))]))

