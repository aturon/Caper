#lang racket

; Provides keywords and syntax classes for reagents

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

(provide-keyword cas! choice match-read update-to!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide reagent-clause reagent-body)

(struct match-read-clause-with-update (pat pre-exps upd-exp post-exps))
(struct match-read-clause-no-update (pat exps))

(define-syntax-class match-read-clause
  #:literals (update-to!)
  (pattern (match-pat pre-upd:expr ...
                      (update-to! new-value:expr)
                      post-upd:expr ...)
           #:attr fragment (match-read-clause-with-update #'match-pat
                                                          (syntax->list #'(pre-upd ...))
                                                          #'new-value
                                                          (syntax->list #'(post-upd ...))))
  (pattern (match-pat e:expr ...)
           #:attr fragment (match-read-clause-no-update #'match-pat
                                                        (syntax->list #'(e ...)))))

(define-syntax-class reagent-clause
  #:literals (cas! choice match-read)
  #:description "define-reagent clause"
  
  (pattern (cas! atomic-ref:expr old-value:expr new-value:expr)
           #:attr fragment (cas!-fragment #'atomic-ref #'old-value #'new-value))

  (pattern (choice [r1:reagent-body] [r2:reagent-body])
           #:attr fragment (choice-of-fragments (attribute r1.fragment)
                                                (attribute r2.fragment)))

  (pattern (match-read atomic-ref:expr clause:match-read-clause ...)
           #:attr fragment (match-read-fragment #'atomic-ref (attribute clause.fragment))))
  
(define-splicing-syntax-class reagent-body
  (pattern (~seq c:reagent-clause ...)
           #:attr fragment (apply sequence-fragments (attribute c.fragment))))