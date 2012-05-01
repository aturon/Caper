#lang racket/base

; a by-hand implementation of Treiber's stack, using mutable cons cells

(require racket/future)
(provide make-treiber-stack push pop)

(struct tstack (head))

(define (make-treiber-stack)
  (tstack (box '())))

(define (push s x)
  (define head (tstack-head s))
  (define node (mcons x '()))
  (let retry ()
    (define snapshot (unbox head))
    (set-mcdr! node snapshot)
    (unless (box-cas! head snapshot node)
      (retry))))

(define (pop s 
	     [failure-result 
	      (λ () (raise-type-error 'pop "non-empty stack" s))])
  (define head (tstack-head s))
  (let retry ()
    (define snapshot (unbox head))
    (cond
      [(eq? snapshot '()) 
       (if (procedure? failure-result) (failure-result) failure-result)]
      [(box-cas! head snapshot (mcdr snapshot))
       (mcar snapshot)]
      [else 
       (retry)])))


