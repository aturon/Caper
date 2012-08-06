#lang racket

(require racket/future
	 racket/match
         caper/core/reagent 
	 caper/core/atomic-ref)
(provide make-queue enq deq enq! deq!)

(struct node (data next)) ; next is of type atomic-ref(#f U node)
(struct queue (head tail))

(define (internal-make-node data)
  (node data (atomic-ref #f)))

(define (make-queue)
  (define n (internal-make-node #f))
  (queue (atomic-ref n) (atomic-ref n)))

(define-match-expander aref
  (λ (stx)
    (syntax-case stx ()
      [(_ p) #'(app atomic-ref-read p)])))

(define-reagent (deq q failure-result)
  (read-match (queue-head q)
    [(node _ (aref (and n (node x _)))) 
     (update-to! n) 
     x]
    [emp (if (procedure? failure-result) (failure-result) failure-result)]))

(define (find q)
  (define tail-aref (queue-tail q))
  (let loop ()
    (match (atomic-ref-read tail-aref)
      [(node _ (and r (aref #f)))
       r]
      [(and ov (node _ (aref nv)))
       (atomic-ref-cas! tail-aref ov nv)
       (loop)])))

(define-reagent (enq q x)
  (prelude (define n (internal-make-node x)))
  (cas! (find q) #f n)
;  (postlude (atomic-ref-cas! (queue-tail q) n))
)

(define (enq! q x) (react (enq q x)))
(define (deq! q [failure-result 
		 (λ () (raise-type-error 'deq! "non-empty queue" q))])
  (react (deq q failure-result)))
