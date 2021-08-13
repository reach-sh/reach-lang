#lang racket/base
(require racket/runtime-path
         racket/path
         racket/file
         racket/pretty
         racket/match
         lang-file/read-lang-file)

(define-runtime-path scrbl "../docs-src")

(define d display)

(define (code c [lang #f])
  ;; XXX lang
  (d "`")
  (for-each d c)
  (d "`"))

(define (code-block l [lang #f])
  (d "```")
  (when lang (d lang))
  (d "\n")
  (for-each d l)
  (d "\n```\n"))

(define (header lvl opts)
  (define hash-lvl (make-string lvl #\#))
  (define tag "")
  (define name '())
  (let loop ([o opts])
    (match o
      [`(#:version ,_ ,@o)
        (loop o)]
      [`(#:style ,_ ,@o)
        (loop o)]
      [`(#:tag ,t ,@o)
        (set! tag (format "{#~a} " t))
        (loop o)]
      [(cons (? list?) _)
       (set! name o)]
      [(cons (? string?) _)
       (set! name o)]))
  (d (format "~a ~a" hash-lvl tag))
  (egol name))

(define ego
  (match-lambda
    [(? string? s) (d s)]
    [`(tech ,t) (ego t)]
    [`(link ,t ,l)
      (d (format "[~a](~a)" l t))]
    [`(reachexlink ,f)
      (d (format "<GitLink href=\"/examples/~a\">`~a`</GitLink>" f f))]
    [`(title ,@o) (header 1 o)]
    [`(section ,@o) (header 2 o)]
    [`(subsection ,@o) (header 3 o)]
    [`(subsubsection ,@o) (header 4 o)]
    [`(seclink ,t ,l)
      (d (format "[~a](##~a)" l t))]
    [`(reachin ,@c) (code c 'reach)]
    [`(jsin ,@c) (code c 'js)]
    [`(pyin ,@c) (code c 'py)]
    [`(goin ,@c) (code c 'go)]
    [`(author (author+email ,a ,e)) (void)]
    [`DApp (d "DApp")]
    [`DApps (d "DApps")]
    [`(hrule) (d "---")]
    [`RPS (d "_Rock, Paper, Scissors!_")]
    [`(emph . ,c) (d "_") (egol c) (d "_")]
    [`(italic . ,c) (d "_") (egol c) (d "_")]
    [`(bold . ,c) (d "**") (egol c) (d "**")]
    [`(tt ,@c) (code c)]
    [`(exec ,@c) (code c)]
    [`(envref ,@c) (code c)]
    [`(defconmode ,@c) (code c)]
    [`(defenv ,@c) (code c)]
    [`(envvar ,@c) (code c)]
    [`(nonterm ,@c) (code c)]
    [`(cmd ,@c)
      (d "```\n$ ") (egol c) (d "\n```\n")]
    [`(Flag ,@c) (code (cons "-" c))]
    [`(DFlag ,@c) (code (cons "--" c))]
    [`(filepath ,@c) (code c)]
    [`(litchar ,@c) (code c)]
    [`(local-table-of-contents . ,_) (d "[[toc]]")]
    [`(table-of-contents . ,_) (d "[[toc]]")]
    [`(element (make-style #f (list (url-anchor ,a))) '())
      (d (format "<a name=~s></a>" a))]
    [`(require . ,_) (void)]
    [`(mint-scope . ,_) (void)]
    [`(define . ,_) (void)]
    [`(include-section . ,_) (void)]
    [`(index-section . ,_) (void)]
    [`(index . ,_) (void)]
    [`(reach . ,l) (code-block l 'reach)]
    [`(go . ,l) (code-block l 'go)]
    [`(js . ,l) (code-block l 'js)]
    [`(py . ,l) (code-block l 'py)]
    [`(verbatim . ,l) (code-block l #f)]
    [`(,(or 'itemize 'itemlist) #:style 'ordered ,@l)
      (for ([i (in-naturals 1)]
            [ei (in-list l)])
        (d i) (d ". ")
        (ego ei))]
    [`(,(or 'itemize 'itemlist) ,@l)
      (for ([ei (in-list l)])
        (d "+ ") (ego ei))]
    [`(item ,@l)
      (egol l) (d "\n")]
    [`(margin-note . ,l)
      (d "::: note\n")
      (egol l)
      (d "\n:::")]
    [(or
       `(reachex ,f 'only ,from ,to ,_)
       `(reachex #:mode ,_ ,f 'only ,from ,to ,_))
      (d (format "@[code{~a-~a}](@examples/~a)" from to f))]
    [x
      (set-box! BAD #t)
      (define xs (pretty-format x #:mode 'write))
      (eprintf "XXX ~a\n" (string-limit xs 70))
      (d "XXX ")
      (d xs)]))

(define (string-limit s n)
  (substring s 0 (min n (string-length s))))

(define (egol l)
  (for-each ego l))

(define (go! sp)
  (match-define
    `(module ,name ,_ (#%module-begin doc ,@cs))
    (syntax->datum (read-lang-file sp)))
  (define mp (format "src/~a.md" name))
  (with-output-to-file mp
    #:exists 'replace
    (lambda ()
      (egol cs))))

(define BAD (box #f))
(module+ main
  (for ([bn (sort (directory-list scrbl)
                  #:key path->string string-ci<=?)])
    (define p (build-path scrbl bn))
    (when (equal? #".scrbl" (path-get-extension bn))
      (set-box! BAD #f)
      (go! p)
      (when (unbox BAD)
        (eprintf "^ >>> ~a\n" bn)))))
