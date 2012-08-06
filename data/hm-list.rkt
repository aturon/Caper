#lang racket

; A variant of Harrs-Michael linked lists,
; used to represent finite mappings

(require racket/future racket/match caper/core/reagent caper/core/atomic-ref)

; special, unexported values for the val field of a node
(define deleted (gensym)) ; THIS node is deleted
(define marked  (gensym)) ; the PREDECESSOR of this node is deleted

; key:  any
; val:  atomic-ref(any) (with deleted and marked as special values)
; next: atomic-ref(node U ())
(struct node (key val next))

(struct hm-list (head <))

; mark: node -> bool
; attempt to append a marker to the given node.
; returns true if successful
(define (mark n)
  (atomic-ref-cas! (node-next n)))

; helps finish an in-progress deletion by linking in a marked node
; or unlinking from a predecessor.
; pred: the predecessor of n
; n: the node to delete
(define (help-delete pred n)
  ; from JUC: "rechecking links and then doing only one of the help-out stages
  ; per call tends to minimize CAS interference among helping threads."
  (define pred-next-ref (node-next pred))
  (when (eq? (atomic-ref-read pred-next-ref) n)
    (define n-next-ref (node-next n))
    (define succ (atomic-ref-read n-next-ref))
    (if (or (eq? succ '()) (not (eq? (atomic-ref-read (node-val succ)) marked)))
	; not already mark, attempt to mark
	(atomic-ref-cas! next-ref succ (node marked marked succ))

	; already marked, attempt to physically remove
	(atomic-ref-cas! pred-next-ref n (atomic-ref-read (node-next succ))))))

(define-reagent (find-window hm from key)
  (let loop ([cur from])
    (define n (atomic-ref-read (node-next cur)))
    (if (null? n)
	(values cur n)
	(let ([v (atomic-ref-read (node-value n))])
	  (cond [(eq? v deleted)   ; n is deleted
		 (help-delete cur n) (retry)]
		[(eq? v marked)	   ; cur is deleted
		 (retry)]
		[((hm-list-< hm) (node-key n) key)
		 (find-window hm n key)]
		[else 
		 (values cur n)])))))
#|

(define-reagent (hm-list-ref hm key failure-result)
  (bind-values (pred n) (find-window hm (atomic-ref-read (hm-head hm)) key))
  (if (null? n) 
      failure-result
      
#|
