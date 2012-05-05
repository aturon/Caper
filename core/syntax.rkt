#lang racket

(require syntax/parse "fragment.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keywords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; cribbed from class-internal.rkt
(define-for-syntax (do-keyword stx orig-sym)
  (let ([orig-stx (datum->syntax #f orig-sym stx)])
    (if (identifier? stx)
        (raise-syntax-error
         #f
         "illegal (unparenthesized) use of a define-reagent keyword"
         orig-stx)
        (raise-syntax-error
         #f
         "use of a define-reagent keyword is not in a define-reagent top-level"
         orig-stx))))

(define-syntax provide-keyword
  (syntax-rules ()
    [(_ id ...)
     (begin
       (define-syntax (id stx) (do-keyword stx 'id))
       ...
       (provide id ...))]))

(provide-keyword cas! choice match-read update-to)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide reagent-clause reagent-body)

(define-syntax-class reagent-clause
  #:literals (cas! choice)
  #:description "define-reagent clause"
  
  (pattern (cas! atomic-ref:expr old-value:expr new-value:expr)
           #:attr fragment (cas!-fragment #'atomic-ref #'old-value #'new-value))

  (pattern (choice [r1:reagent-body] [r2:reagent-body])
           #:attr fragment (choice-of-fragments (attribute r1.fragment)
                                                (attribute r2.fragment))))
  
(define-splicing-syntax-class reagent-body
  (pattern (~seq c:reagent-clause ...)
           #:attr fragment (apply sequence-fragments (attribute c.fragment))))