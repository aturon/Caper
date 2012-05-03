#lang racket/base

; a by-hand implementation of Treiber's stack, using mutable cons cells

(require racket/future racket/performance-hint racket/unsafe/ops)
(provide make-treiber-stack push push* pop pop*)

(struct tstack (head))

(define (make-treiber-stack)
  (tstack (box '())))

#;
(begin
  (define-syntax-rule (alloc-node x) (mcons x '()))
  (define-syntax-rule (update-node node x ss)
    (unsafe-set-mcdr! node ss))
  (define-syntax-rule (hd x) (unsafe-mcar x))
  (define-syntax-rule (tl x) (unsafe-mcdr x)))


(begin
  (define-syntax-rule (alloc-node x) (cons x '()))
  (define-syntax-rule (update-node node x ss) 
    (define node (cons x ss)))
  (define-syntax-rule (hd x) (unsafe-car x))
  (define-syntax-rule (tl x) (unsafe-cdr x)))


(define-syntax-rule (push* s x)
  (let ()
    (define _x x)
    (define head (tstack-head s))
    (define node (alloc-node _x))
    (let retry ()
      (define snapshot (unsafe-unbox* head))
      (update-node node _x snapshot)
      (unless (unsafe-box*-cas! head snapshot node)
        (retry)))))

(define (push s x)
  (define head (tstack-head s))
  (define node (alloc-node x))
  (let retry ()
    (define snapshot (unsafe-unbox* head))
    (update-node node x snapshot)
    (unless (unsafe-box*-cas! head snapshot node)
      (retry))))

(define (pop s 
             [failure-result 
              (λ () (raise-type-error 'pop "non-empty stack" s))])
  (define head (tstack-head s))
  (let retry ()
    (define snapshot (unsafe-unbox* head))
    (cond
      [(eq? snapshot '()) 
       (if (procedure? failure-result) (failure-result) failure-result)]
      [(box-cas! head snapshot (tl snapshot))
       (hd snapshot)]
      [else 
       (retry)])))


(define-syntax-rule (pop* s)
  (let ()
    [define failure-result 
      (λ () (raise-type-error 'pop "non-empty stack" s))]
    (define head (tstack-head s))
    (let retry ()
      (define snapshot (unsafe-unbox* head))
      (cond
        [(eq? snapshot '()) 
         (if (procedure? failure-result) (failure-result) failure-result)]
        [(box-cas! head snapshot (tl snapshot))
         (hd snapshot)]
        [else 
         (retry)]))))


