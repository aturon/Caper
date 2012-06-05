#lang racket

(require racket/future
         caper/core/reagent caper/core/atomic-ref)

(struct node (data next))
(struct queue (head tail))

(define (make-queue)
  (define n (node #f #f))
  (queue (atomic-ref n) (atomic-ref n)))
#;
(define-reagent (try-deq q)
  (read-match q
    [(node _ r) (read-match r [(and n (node x _))
                               (update-to! n)
                               x])]
    [emp (update-to! emp) #f]))

(define (find q)
  (let loop ()
    (reagent (read-match (queue-tail q)
               [(and ov (node _ (and r (app atomic-ref-read #f))))
                r]
               [(and ov (node _ (and r (app atomic-ref-read nv))))
                (reagent (cas! (queue-tail q) ov nv))
                (loop)]))))

(define-reagent (enq q x)
  (define r (find q))
  (cas! r #f x)
  ;; post-commit here
  )

