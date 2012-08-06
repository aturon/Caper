#lang racket

(require racket/future
         caper/core/reagent 
	 caper/core/atomic-ref
	 caper/data/ms-q)
(provide sync-ref sync-ref?)

(define (sync-ref init)
  (define q (make-queue))
  (define (enroll offer)
    (enq! q offer))
  (define (signal-all)
    (match (deq! q #f)
      [#f    (void)]
      [offer (fsemaphore-post offer)
	     (signal-all)]))
  (blocking-atomic-ref init (waiter-group enroll signal-all)))

(define (sync-ref? sr)
  (blocking-atomic-ref? sr))