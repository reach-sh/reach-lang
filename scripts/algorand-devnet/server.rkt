#lang racket/base
(require net/url
         racket/runtime-path
         racket/file
         web-server/http
         web-server/servlet-env
         web-server/http/empty)

(define-runtime-path output-dir "debug")
(make-directory output-dir)

(define i 0)
(define (do-log)
  (define req (thread-receive))
  (printf "~a ~a ~a\n" i (request-method req) (url->string (request-uri req)))
  (flush-output)
  (define bs (request-post-data/raw req))
  (define f (build-path output-dir (number->string i)))
  (display-to-file bs f)
  (set! i (add1 i))
  (do-log))
(define logger (thread do-log))

(define (start req)
  (thread-send logger req)
  (response/empty #:code 200))

(module+ main
  (serve/servlet
    #:command-line? #t
    #:port 9392
    #:listen-ip #f
    #:servlet-regexp #rx""
    start))
