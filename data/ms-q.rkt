#lang racket

(require racket/future
         caper/core/reagent caper/core/atomic-ref)

(struct node (data next))
(struct queue (head tail))

(define (make-queue)
  (define n (node #f #f))
  (queue (atomic-ref n) (atomic-ref n)))

(define-match-expander aref
  (λ (stx)
    (syntax-case stx ()
      [(_ p) #'(app atomic-ref-read p)])))

(define-reagent (try-deq q)
  (read-match (queue-head q)
    [(node _ (aref (and n (node x _)))) 
     (update-to! n)]
    [emp (update-to! emp) #f]))

(define (find q)
  (let loop ()
    (reagent 
     (read-match (queue-tail q)
       [(node _ (and r (aref #f)))
        r]
       [(and ov (node _ (aref nv)))
        (reagent (cas! (queue-tail q) ov nv))
        (loop)]))))

(define-reagent (enq q x)  
  (cas! (find q) #f x)
  ;; post-commit here
  )

