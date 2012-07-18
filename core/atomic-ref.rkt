#lang racket

; a specialized form of box usable within reagents; allows KCAS operations

(require racket/future racket/unsafe/ops
         (for-syntax syntax/parse))
(provide atomic-ref
         atomic-ref?
         atomic-ref-read
         atomic-ref-cas!
         atomic-ref-box ;; internal library use only!
	 static-kcas!   ;; internal library use only!
         unsafe-atomic-ref-box  ;; internal use only!
	 flatten-mixed-kcas
)

; internal, generative value for kcas acquisition of an atomic-ref
(define locked (gensym))

(struct atomic-ref (box)
  #:constructor-name internal-make-atomic-ref
  #:omit-define-syntaxes)

(define-syntax-rule (unsafe-atomic-ref-box b)
  (unsafe-struct-ref b 0))

; atomic-ref: any/c -> atomic-ref?
; constructs an atomic reference holding some initial value
(define (atomic-ref init)
  (internal-make-atomic-ref (box init)))

; atomic-ref-read: atomic-ref? -> any/c
; snapshots the current value of an atomic-ref
(define (atomic-ref-read ar)
  (define b (atomic-ref-box ar))
  (let retry ()
    (match (unsafe-unbox* b)
      [(== locked eq?) (retry)] ; re-read until no pending kcas
      [v v])))

; atomic-ref-cas!: atomic-ref? any/c any/c -> boolean?
; (atomi-ref-cas! ar ov nv) attempts atomically update the atomic-ref ar from ov
; to nv, returning a boolean indicating success
(define (atomic-ref-cas! ar ov nv)
  (unsafe-box*-cas! (atomic-ref-box ar) ov nv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; k-word compare-and-set implementation

(struct kcas-item (box ov nv))

; currently we take a dirt-simple, obstructable approach
(define (kcas! items)
  ; attempts to cas every ref in items to `locked'; stops on the first failure.
  ; returns the suffix of unsuccessful items
  (define (acquire items)
    (match items
      ['() '()]
      [(cons (kcas-item b ov nv) rest)
       (if (unsafe-box*-cas! b ov locked)
           (acquire rest)
           items)]))
  ; assumes: end is a suffix of the list start
  ; rolls back every item in start that is not in end
  (define (roll-back-until start end)
    (unless (eq? start end)
      (match start
        [(cons (kcas-item b ov nv) rest)
         (begin (unsafe-set-box*! b ov)
                (roll-back-until rest end))])))
  ; update every item to its desired new value
  (define (commit items)
    (for-each (match-lambda [(kcas-item b ov nv)
                             (unsafe-set-box*! b nv)])
              items))
  ; special-case zero and one-item cas lists
  (match items
    ['()
     #t]
    [(list (kcas-item ar ov nv))
     (atomic-ref-cas! ar ov nv)]
    [_ (match (acquire items)
         ['() (commit items)]
         [unacquired (roll-back-until items unacquired)])]))

(define-syntax (flatten-mixed-kcas stx)
  (syntax-parse stx
    [(_) #'null]
    [(_ (b ov nv) rest ...)
     #'(cons (kcas-item b ov nv) (flatten-mixed-kcas rest ...))]
    [(_ dyn rest ...)
     #'(append dyn (flatten-mixed-kcas rest ...))]))

; for internal use only
; not currently making much use of static information
(define-syntax (static-kcas! stx)  
  (syntax-parse stx
    [(_) #'#t]
    [(_ (b ov nv))
     #'(unsafe-box*-cas! b ov nv)]
    [(_ clause ...)
     #'(kcas! (flatten-mixed-kcas clause ...))]))