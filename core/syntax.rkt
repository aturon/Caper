#lang racket

; Provides keywords and syntax classes for reagents

(require syntax/parse "fragment.rkt" 
	 (for-template "keywords.rkt"))
(provide reagent-clause reagent-body)

(define-syntax-class read-match-clause
  #:literals (update-to!)
  (pattern (match-pat pre:reagent-body
                      (update-to! new-value:expr)
                      post:reagent-body)
           #:attr fragment 
	          (read-match-clause-with-update #'match-pat 
						 (attribute pre.fragment)
						 #'new-value
						 (attribute post.fragment)))
  (pattern (match-pat body:reagent-body)
           #:attr fragment 
                  (read-match-clause-no-update #'match-pat
					       (attribute body.fragment))))

(define-syntax-class reagent-clause
  #:literals (cas! choice read-match)
  #:description "define-reagent clause"
  
  (pattern (cas! atomic-ref:expr old-value:expr new-value:expr)
           #:attr fragment (cas!-fragment #'atomic-ref #'old-value #'new-value))

  (pattern (choice [r1:reagent-body] [r2:reagent-body])
           #:attr fragment (choice-of-fragments (attribute r1.fragment)
                                                (attribute r2.fragment)))

  (pattern (read-match atomic-ref:expr clause:read-match-clause ...)
           #:attr fragment (read-match-fragment #'atomic-ref (attribute clause.fragment))))
  
(define-splicing-syntax-class reagent-body
  (pattern (~seq c:reagent-clause ...)
           #:attr fragment (apply sequence-fragments (attribute c.fragment))))