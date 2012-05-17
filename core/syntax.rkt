#lang racket

; Provides keywords and syntax classes for reagents

(require syntax/parse 
	 (for-template "fragment.rkt" "keywords.rkt"))
(provide reagent-clause reagent-body)

(define-syntax-class read-match-clause
  #:literals (update-to!)
  (pattern (match-pat pre:reagent-clause ...
                      (update-to! new-value:expr)
                      post:reagent-clause ...)
           #:attr fragment
	          #'(match-pat pre.fragment ...
			       (update-to! new-value)
			       post.fragment ...)))
  ;; (pattern (match-pat f:reagent-clause ...)
  ;;          #:attr fragment 
  ;;                 (#'match-pat (update-to!) f.fragment ...)))

(define-syntax-class reagent-clause
  #:literals (cas! choice read-match)
  #:description "define-reagent clause"
  
  (pattern (cas! atomic-ref:expr old-value:expr new-value:expr)
           #:attr fragment #'(cas!-fragment atomic-ref old-value new-value))

  (pattern (choice [r1:reagent-body] [r2:reagent-body])
           #:attr fragment #'(choice r1.fragment r2.fragment))

  (pattern (read-match atomic-ref:expr clause:read-match-clause ...)
           #:attr fragment #'(read-match-fragment atomic-ref clause.fragment ...))

  (pattern e:expr
	   #:attr fragment #'(continue-with e)))

(define-splicing-syntax-class reagent-body
  (pattern (~seq c:reagent-clause ...)
           #:attr fragment #'(sequence c.fragment ...)))