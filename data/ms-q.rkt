#lang racket

(require racket/future
	 racket/match
         caper/core/reagent 
	 caper/core/atomic-ref)
(provide make-queue enq deq enq! deq!)

; NB: next is of type atomic-ref(#f U node)
(struct node (data next))
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
  (let loop ()
    (match (atomic-ref-read (queue-tail q))
      [(node _ (and r (aref #f)))
       r]
      [(and ov (node _ (aref nv)))
       (reagent (cas! (queue-tail q) ov nv))
       (loop)])))

(define-reagent (enq q x)  
  (cas! (find q) #f (internal-make-node x))
  ;; post-commit here
  )

(define (enq! q x) (react (enq q x)))
(define (deq! q [failure-result 
		 (λ () (raise-type-error 'deq! "non-empty queue" q))])
  (react (deq q failure-result)))
  