#lang racket

; Provides syntax classes for reagents

(require syntax/parse racket/syntax
	 (for-template racket/base (prefix-in sem: "semantics.rkt") "keywords.rkt"))
(provide (struct-out static-reagent) (struct-out reagent-macro) reagent-clause reagent-body reagent-macro)

(struct static-reagent (formals prelude payload)
  #:property prop:procedure
  ;; this is what happens when a statically-bound reagent is referenced *outside* of 
  ;; any other reagent computation -- ie, as a function
  (lambda (r stx) 
    (syntax-parse stx
      ;; redirect to the second case
      [(f actuals ...) 
       #`(#,(datum->syntax stx '#%app) f actuals ...)]
      [_ #`(λ #,(static-reagent-formals r)             
             (#%reify (begin #,@(static-reagent-prelude r)) #,(static-reagent-payload r)))])))

(struct reagent-macro (trans)
  #:property prop:procedure (λ (stx) (raise-syntax-error #f "must be used inside a reagent" stx)))

(define-syntax-class reagent-clause
  #:literals (#%cas! #%choose #%read #%dynamic)
  #:description "define-reagent clause"
  #:attributes ((prelude 1) payload)

  (pattern (#%cas! box:id old-value:id new-value:id)
           #:with (prelude ...) #'()
           #:attr payload #'(sem:#%cas! box old-value new-value))

  (pattern (#%read box:id)
           #:with (prelude ...) #'()
           #:attr payload #'(sem:#%read box))

  (pattern (#%choose r:reagent-clause s:reagent-clause)
           #:with (prelude ...) #'(r.prelude ... s.prelude ...)
           #:attr payload #'(sem:#%choose r.payload s.payload))
  
  (pattern ((~literal prelude) e:expr ...)
           #:with (prelude ...) #'(e ...)
           #:attr payload #'(sem:#%return (void)))

  (pattern ((~literal postlude) e:expr ...)
           #:with (prelude ...) #'()
	   #:attr payload #'(sem:#%postlude (begin e ...)))
  
  (pattern [(~var r (static reagent-macro? "reagent macro")) . args]
           #:attr trans (reagent-macro-trans (attribute r.value))
           #:with new-stx:reagent-clause 
           (let ([intr (make-syntax-introducer)])
             (intr ((attribute trans) (intr this-syntax)))) 
           #:with (prelude ...) #'(new-stx.prelude ...)
           #:with payload #'new-stx.payload)
  
  (pattern [(~var r (static static-reagent? "static reagent")) args:expr ...]
           #:with (formals ...) (static-reagent-formals (attribute r.value))
           #:with (prelude ...) #'()
           ;; the prelude of `r` can refer to `formals`, so it must go *inside* the `let`
           ;; TODO: can we discover optimization opportunities for lifting the prelude higher?
           #:with (p ...) (static-reagent-prelude (attribute r.value)) 
           #:attr payload #`(let ([formals args] ...) p ... #,(static-reagent-payload (attribute r.value))))
  
  (pattern (values e:expr ...)
           #:with (prelude ...) #'()
	   #:attr payload #'(sem:#%return e ...))

  (pattern (begin-reagent r:reagent-body)
	   #:with (prelude ...) #'(r.prelude ...)
	   #:with payload #'r.payload)

  (pattern (computed e:expr)
           #:with (prelude ...) #'()
           #:attr payload #'(sem:#%reflect e)))

(define-splicing-syntax-class reagent-body
  #:literals (bind-values)
  #:attributes ([prelude 1] payload)

  (pattern c:reagent-clause
           #:with (prelude ...) #'(c.prelude ...)
           #:attr payload #'c.payload)

  (pattern (~seq (bind-values (x:id ...) c:reagent-clause) rest:reagent-body)
	   #:with (prelude ...) #'(c.prelude ... rest.prelude ...)
	   #:attr payload #'(sem:#%bind c.payload (x ...) rest.payload)))