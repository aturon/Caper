#lang racket

; Provides keywords and syntax classes for reagents

(require syntax/parse racket/syntax "static.rkt"
	 (for-template racket/base "fragment.rkt" "keywords.rkt" "atomic-ref.rkt"))
(provide reagent-clause reagent-body)

(define-syntax-class read-match-clause
  ;; can't lift preludes above here, because they might
  ;; escape their binders
  #:literals (update-to!)
  (pattern (match-pat pre:reagent-clause ...
                      (update-to! new-value:expr)
                      post:reagent-clause ...)
           #:attr fragment
	          #'(match-pat [pre.prelude ... ...
				post.prelude ... ...]
			       pre.fragment ...
			       (update-to! new-value)
			       post.fragment ...))
   (pattern (match-pat f:reagent-clause ...)
            #:attr fragment 
                   #'(match-pat [f.prelude ... ...]
				(update-to!) 
				f.fragment ...)))

(define-syntax-class reagent-clause
  #:literals (cas! choice read-match dynamic)
  #:description "define-reagent clause"
  #:attributes ((prelude 1) fragment)
  (pattern (cas! atomic-ref:expr old-value:expr new-value:expr)
           #:with (bv bx) (generate-temporaries '(bv bx))
           #:attr fragment #'(cas!-fragment bx old-value new-value)
           #:with (prelude ...) 
	   #'((define bv atomic-ref)
	      (unless (atomic-ref? bv)
		(error 'cas! "atomic-ref expected, but got" bv))
              (define bx (atomic-ref-box bv))))

  (pattern (choice [r1:reagent-body] [r2:reagent-body])
           #:attr fragment #'(choose-fragment r1.fragment r2.fragment)
           #:with (prelude ...) #'(r1.prelude ... r2.prelude ...))

  (pattern (read-match atomic-ref:expr clause:read-match-clause ...)
           #:with (bv bx) (generate-temporaries '(bv bx))
           #:attr fragment #'(read-match-fragment bx clause.fragment ...)
           #:with (prelude ...)
	   #'((define bv atomic-ref)
	      (unless (atomic-ref? bv)
		(error 'cas! "atomic-ref expected, but got" bv))
              (define bx (atomic-ref-box bv))))
  
  (pattern ((~literal prelude) e:expr ...)
           #:attr fragment #'(continue-with (void))
           #:with (prelude ...) #'(e ...))

  (pattern ((~literal postlude) e:expr ...)
	   #:attr fragment #'(with-postlude (begin e ...) (continue-with (void)))
           #:with (prelude ...) #'())
  
  (pattern (dynamic e:expr)
           #:with (prelude ...) #'()
           #:attr fragment #'(reflect-fragment e))
  
  (pattern [(~var r (static reagent? "static reagent")) args:expr ...]
           #:with (formals ...) (reagent-formals (attribute r.value))
           #:with (prelude ...) #'()
           ;; the prelude of `r` can refer to `formals`, so it must go *inside* the `let`
           ;; TODO: can we discover optimization opportunities for lifting the prelude higher?
           #:with (p ...) (reagent-prelude (attribute r.value))           
           #:attr fragment #`(let ([formals args] ...) p ... #,(reagent-fragment (attribute r.value))))
  
  (pattern e:expr
	   #:attr fragment #'(continue-with e)
           #:with (prelude ...) #'()))

(define-splicing-syntax-class reagent-body
  #:attributes ([prelude 1] fragment)
  (pattern (~seq c:reagent-clause ...)
           #:attr fragment #'(sequence c.fragment ...)
           #:with (prelude ...) #'(c.prelude ... ...)))