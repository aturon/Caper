(module treiber-stack racket

  (require scheme/future)
  (provide make-treiber-stack push pop)

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

  (define (pop s 
	       [failure-result 
		(lambda () 
		  (error (string-append "pop: stack is empty")))])
    (define head (tstack-head s))
    (let retry ()
      (define snapshot (unbox head))
      (cond
        [(eq? snapshot '()) 
	 (if (proc? failure-result) (failure-result) failure-result)]
	[(box-cas! head snapshot (mcdr snapshot))
	 (mcar snapshot)]
	[else 
	 (retry)])))

)