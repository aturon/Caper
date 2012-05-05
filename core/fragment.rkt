#lang racket

(require (for-syntax "atomic-ref.rkt")
         racket/syntax)
(provide empty-fragment cas!-fragment sequence-fragments choice-of-fragments)

(define (empty-fragment
         k retry-k block-k kcas-list)
  (k retry-k block-k (void) kcas-list))

(define ((cas!-fragment atomic-ref-exp old-value-exp new-value-exp)
         k retry-k block-k kcas-list)
  (with-syntax* ([(b ov nv) (generate-temporaries '(b ov ov))]
                 [finish (k retry-k block-k (void) (cons #'(b ov nv) kcas-list))])
    #`(begin (define b  (atomic-ref-box #,atomic-ref-exp))
	     (define ov #,old-value-exp)
	     (define nv #,new-value-exp)
	     finish)))

;; (define ((match-read-fragment box-exp clauses) k retry-k block-k kcas-list)
;;   (with-syntax ([(b ov nv) (generate-temporaries '(b ov ov))])
;;     (define (interpret-clause clause)
;;       (syntax-parse clause #:literals (update-to)
;;         [(pat body-before ... (update-to nv-exp) body-after ...)
;;          (pat 

(define (sequence-fragments . fs)
  ; this can probably be defined more intelligently...
  (define ((sequence-binary f1 f2)
           k retry-k block-k kcas-list)
    (f1 (lambda (retry-k block-k result kcas-list)
          (f2 k retry-k block-k kcas-list))
        retry-k block-k kcas-list))
  (foldr sequence-binary empty-fragment fs))

(define ((choice-of-fragments f1 f2)
         k retry-k block-k kcas-list)
  (with-syntax* ([(alt alt-with-retry) (generate-temporaries '(alt alt-with-retry))]
                 [first-body           (f1 k #'(alt-with-retry) #'(alt) kcas-list)]
                 [alt-body             (f2 k retry-k block-k kcas-list)]
                 [alt-with-retry-body  (f2 k retry-k retry-k kcas-list)])
    #'(begin (define (alt) alt-body)
	     (define (alt-with-retry) alt-with-retry-body)
	     first-body)))