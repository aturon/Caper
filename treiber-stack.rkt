(module treiber-stack racket

  (require scheme/future)
  (provide make-treiber-stack push try-pop)

  (struct tstack (head))

  (define (make-treiber-stack)
    (tstack (box '())))

  (define (push s x)
    (define head (tstack-head s))
    (define node (mcons x '()))
    (let retry ()
      (define snapshot (unbox head))
      (set-mcdr! node snapshot)
      (unless (box-cas! head snapshot node)
	(retry))))

  (define (try-pop s x)
    (define head (tstack-head s))
    (let retry ()
      (define snapshot (unbox head))
      (if (box-cas! head snapshot (mcdr snapshot))
	  (mcar snapshot)
	  (retry))))

)