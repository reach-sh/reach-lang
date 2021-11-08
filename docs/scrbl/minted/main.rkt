#lang racket/base
(require racket/match
         racket/system
         racket/port
         racket/list
         racket/hash
         racket/string
         racket/file
         racket/runtime-path
         file/sha1
         (prefix-in xml: xml)
         scribble/html-properties
         scribble/core
         scribble/base)
(provide mint
         mint-scope
         mint-define!)

(define (system*-maybe bin . args)
  (let ([res (apply system*/exit-code bin args)])
    (if (not (zero? res))
      (error (format "Error running ~a, which returned the error code" bin) res)
      res)))

(define-syntax-rule (io in f)
  (with-output-to-string
    (λ ()
      (with-input-from-string in
        (λ ()
          f)))))

(define pygmentize-bin
  (find-executable-path "pygmentize"))

(define-runtime-path cache "cache")
(define (with-cache kv f)
  (make-directory* cache)
  (define kb (with-output-to-bytes (lambda () (write kv))))
  (define k (sha1 kb))
  (define kp (build-path cache k))
  (cond
    [(file-exists? kp)
     (file->value kp)]
    [else
      (define v (f))
      (write-to-file v kp #:exists 'replace)
      v]))

(define (pygmentize
         #:lang lang
         #:options [opts (hasheq)]
         content)
  (with-cache
    (list content lang opts)
    (lambda ()
      (io content
        (apply system*-maybe
             pygmentize-bin
             "-l" lang
             "-f" "html"
             (append*
              (for/list ([(k v) (in-hash opts)])
                (list "-O" (format "~a=~a" k v)))))))))

(define mint-tag-claimed? (make-hash))
(define (xml->scribble #:get-tag&may [get-tag&may (λ (c s) (values #f #f))] xs)
  (define (wrap tag attrs sty content)
    (match* (tag attrs content)
      [('span (list (cons 'class c)) s)
       (define-values (mint-tag can-define?) (get-tag&may c s))
       (and
        mint-tag
        (cond
          [(or (not can-define?)
               (hash-has-key? mint-tag-claimed? mint-tag))
           (link-element sty s mint-tag)]
          [else
           (hash-set! mint-tag-claimed? mint-tag #t)
           (toc-target2-element #f (as-index (element sty s))
                                mint-tag (tt s))]))]
      [(_ _ _) #f]))
  (define (xexpr->scribble xe)
    (match xe
      [(list* tag attrs content)
       (define attrs-p (map (λ (x) (cons (first x) (second x))) attrs))
       (define s (style #f (list (alt-tag (symbol->string tag)) (attributes attrs-p))))
       (define (mk c)
         (element s c))
       (cond
         [(wrap tag attrs-p s content) => (λ (x) x)]
         [else
          (mk (map xexpr->scribble content))])]
      [(? string? x) x]
      [x (element (style #f (list (xexpr-property x ""))) "")]))

  (define x (xml:string->xexpr xs))
  (xexpr->scribble x))

(define mint-inline-options
  (hasheq 'verboptions "baseline=t"
          'nowrap "True"
          'lineseparator "<br />"))

(define mint-scope (make-parameter #f))

(define mint-defines-once-box (box #f))
(define (mint-define! . defs)
  (set-box! mint-defines-once-box defs))
(define (get-once)
  (begin0 (unbox mint-defines-once-box)
    (set-box! mint-defines-once-box #f)))

(define (mint #:defines [defines (get-once)]
              #:inline? [inline? #f]
              #:options [opts (hasheq)]
              #:scope [scopee (mint-scope)]
              lang . contentl)
  (define content (apply string-append contentl))
  (define opts-p
    (if inline?
      (hash-union mint-inline-options opts)
      opts))
  (define (adjust-inline x)
    (if (not inline?) x
        (format "<code class=\"highlight\">~a</code>"
                ;; Remove trailing <br /> for inline
                (string-trim x "<br />" #:left? #f))))

  (define scope
    '()
    ;; XXX This is too aggressive in JS.
    #;
    (if scopee (list scopee) '()))
  (define langs (string->symbol lang))
  (define (mktag x) (list langs x))
  (define (get-tag c s)
    (match c
      [(or "kd" "kc" "o" "kr" "nb" "k")
       (mktag (list s))]
      [(or "nx")
       (mktag (cons s scope))]
      [_ #f]))
  (define (get-tag&may c s)
    (values (get-tag c s)
            (if (list? defines) (member s defines)
                defines)))

  (xml->scribble
   #:get-tag&may get-tag&may
   (adjust-inline
    (pygmentize
     #:lang lang
     #:options opts-p
     content))))
