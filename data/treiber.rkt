#lang racket/base

; a reagent implementation of Treiber's stack, using cons cells

(require racket/future racket/performance-hint racket/unsafe/ops caper/core/reagent caper/core/atomic-ref) 
(provide make-treiber-stack push pop)

(struct tstack (head)
  #:property prop:custom-write
  (λ (v p mode)
    ((case mode
       [(#t) write]
       [(#f) display]
       [else (λ (p port) (print p port 0))])
    `(tstack ,@(unbox (atomic-ref-box (tstack-head v))))
     p)))

(define (make-treiber-stack)
  (tstack (atomic-ref null)))

(define-reagent (push s x)
  (read-match (tstack-head s)
    [xs (update-to! (cons x xs))]))

(define-reagent (pop s [failure-result 
                        (λ () (raise-type-error 'pop "non-empty stack" s))])
  (read-match (tstack-head s)
    [(cons x xs) (update-to! xs) x]
    [_ (if (procedure? failure-result) (failure-result) failure-result)]))
