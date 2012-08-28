#lang racket

; Provides syntax classes for reagents

(require syntax/parse racket/syntax
	 (for-template racket/base (prefix-in sem: "semantics.rkt") "keywords.rkt"))
(provide (struct-out static-reagent) (struct-out reagent-macro)
         reagent-clause reagent-body reagent-macro reagent-prelude)

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
             (sem:#%reify (begin #,@(static-reagent-prelude r)) #,(static-reagent-payload r)))])))

(struct reagent-macro (trans)
  #:property prop:procedure (λ (stx) (raise-syntax-error #f "must be used inside a reagent" stx)))

(define-syntax-class reagent-macro-application
  #:attributes (transformed)

  (pattern [(~var r (static reagent-macro? "reagent macro")) . args]
           #:attr transform (reagent-macro-trans (attribute r.value))
           #:with transformed
           (let ([intr (make-syntax-introducer)])
             (intr ((attribute transform) (intr this-syntax))))))

(define-syntax-class reagent-clause
  #:literals (#%cas! #%choose #%read #%match block retry values begin-reagent computed)
  #:description "define-reagent clause"
  #:attributes (payload)

  (pattern m:reagent-macro-application
           #:with new-clause:reagent-clause #'m.transformed
           #:attr payload #'new-clause.payload)

  (pattern (#%cas! box:id old-value:id new-value:id)
           #:attr payload #'(sem:#%cas! box old-value new-value))

  (pattern (#%read box:id)
           #:attr payload #'(sem:#%read box))

  (pattern (#%choose r:reagent-clause s:reagent-clause)
           #:attr payload #'(sem:#%choose r.payload s.payload))

  (pattern (#%match x:id [pat r:reagent-body] ...)
           #:attr payload #'(sem:#%match x [pat r.payload] ...))

  (pattern (block)
           #:attr payload #'(sem:#%block))

  (pattern (retry)
           #:attr payload #'(sem:#%retry))
  
  (pattern ((~literal postlude) e:expr ...)
	   #:attr payload #'(sem:#%postlude (begin e ...)))
  
  (pattern [(~var r (static static-reagent? "static reagent")) args:expr ...]
           #:with (formals ...) (static-reagent-formals (attribute r.value))
           ;; the prelude of `r` can refer to `formals`, so it must go *inside* the `let`
           ;; TODO: can we discover optimization opportunities for lifting the prelude higher?
           #:with (p ...) (static-reagent-prelude (attribute r.value)) 
           #:attr payload #`(let ([formals args] ...) p ... #,(static-reagent-payload (attribute r.value))))
  
  (pattern (values e:expr ...)
	   #:attr payload #'(sem:#%return e ...))

  (pattern (begin-reagent r:reagent-body)
	   #:with payload #'r.payload)

  (pattern (computed e:expr)
           #:attr payload #'(sem:#%reflect e)))

(define-splicing-syntax-class reagent-body
  #:literals (bind-values)
  #:attributes (payload)

  (pattern (~seq m:reagent-macro-application c ...)
           #:with (new-body:reagent-body) #'(m.transformed c ...)
           #:attr payload #'new-body.payload)
  
  (pattern c:reagent-clause
           #:attr payload #'c.payload)

  (pattern (~seq c:reagent-clause rest:reagent-body)
           #:attr payload #'(sem:#%seq c.payload rest.payload))

  (pattern (~seq (bind-values (x:id ...) c:reagent-clause) rest:reagent-body)
	   #:attr payload #'(sem:#%bind c.payload (x ...) rest.payload)))

(define-splicing-syntax-class reagent-prelude
  #:literals (prelude)
  #:attributes ([payload 1])
  
  (pattern ((~seq prelude e ...))
           #:with (payload ...) #'(e ...))

  (pattern (~seq)
           #:with (payload ...) #'()))