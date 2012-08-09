#lang racket/base

; a reagent implementation of Treiber's stack, using cons cells

(require caper/base/top-level racket/unsafe/ops)
(provide make-treiber-stack push pop push! pop! pop/block pop/block!)

(struct tstack (head))

; TODO: reinstante this
  ;; #:property prop:custom-write
  ;; (λ (v p mode)
  ;;   ((case mode
  ;;      [(#t) write]
  ;;      [(#f) display]
  ;;      [else (λ (p port) (print p port 0))])
  ;;   `(tstack ,@(unbox (atomic-ref-box (tstack-head v))))
  ;;    p)))

(define (make-treiber-stack)
  (tstack (atomic-ref null)))

(define-reagent (push s x)
  (prelude (define c (mcons x null))           
           (define hd (tstack-head s)))
  (read-match hd
    [xs (for-effect (unsafe-set-mcdr! c xs))
        (update-to! c)]))

(define-reagent (pop s failure-result)  
  (read-match (tstack-head s)
    [(mcons x xs) (update-to! xs) (values x)]
    [_ (values (if (procedure? failure-result) (failure-result) failure-result))]))

(define-reagent (pop/block s)
  (read-match (tstack-head s)
    [(mcons x xs) (update-to! xs) (values x)]))


(define (push! s x) (react (push s x)))
(define (pop/block! s) (react (pop/block s)))
(define (pop! s [failure-result 
		 (λ () (raise-type-error 'pop! "non-empty stack" s))])
  (react (pop s failure-result)))