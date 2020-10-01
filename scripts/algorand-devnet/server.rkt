#lang racket/base
(require net/url
         racket/list
         racket/runtime-path
         racket/file
         racket/path
         web-server/http
         web-server/dispatch
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

(define (drop-log req _method)
  (thread-send logger req)
  (response/empty #:code 200))

(define (list-logs req)
  (response/xexpr
    `(html
       (body
         (ul
           ,@(for/list ([wn (in-list
                             (sort
                               (map (lambda (x) (string->number (path->string (file-name-from-path x))))
                                    (directory-list output-dir))
                               <=))])
              (define w (number->string wn))
              `(li (a ([href ,(debug-url view-log w)])
                      ,w))))))))

(define (view-log req which)
  (define p (build-path output-dir which))
  (response/full
    200 #"OK"
    (file-or-directory-modify-seconds p)
    APPLICATION/JSON-MIME-TYPE
    empty
    (list (file->bytes p))))

(define-values (debug-dispatch debug-url)
  (dispatch-rules
    [("") list-logs]
    [("view" (string-arg)) view-log]
    [("exec" (string-arg)) #:method "post" drop-log]))

(module+ main
  (serve/servlet
    #:command-line? #t
    #:port 9392
    #:listen-ip #f
    #:servlet-regexp #rx""
    debug-dispatch))
