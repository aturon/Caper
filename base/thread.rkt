#lang racket

(require (for-syntax syntax/parse) 
         racket/future 
	 racket/stxparam
	 racket/stxparam-exptime 
	 syntax/parse/define)
(provide tl-semaphore fork)

#|
(define tl-semaphore (make-parameter #f))

(define-simple-macro (fork body ...)
  (parameterize ([tl-semaphore (make-fsemaphore 0)])
    (future (lambda () body ...))))
|#

(define tl-semaphore (make-fsemaphore 0))

(define-simple-macro (fork body ...)
  (future (lambda () body ...)))