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
    [`(reachexlink ,f #:dir ,e)
      (d (format "[~a](@github/~a/~a)" f e f))]
    [`(reachexlink ,f)
      (d (format "[~a](@github/examples/~a)" f f))]
    [`(reachexlink ,f ,c #:dir ,e)
      (d "[")
      (ego c)
      (d (format "](@github/~a/~a)" e f))]
    [`(reachexlink ,f ,c)
      (d "[")
      (ego c)
      (d (format "](@github/examples/~a)" f))]
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
    [`(conmode ,@c) (code c)]
    [`(defenv ,@c) (code c)]
    [`(envvar ,@c) (code c)]
    [`(nonterm ,@c) (code c)]
    [`(cmd ,@c)
      (d "```\n$ ") (egol c) (d "\n```\n")]
    [`(Flag ,@c) (code (cons "-" c))]
    [`(DFlag ,@c) (code (cons "--" c))]
    [`(filepath ,@c) (code c)]
    [`(litchar ,@c) (code c)]
    [`(the-community-link)
      ;; XXX
      (d "<CommunityLink />")]
    [`(local-table-of-contents . ,_) (d "[[toc]]")]
    [`(table-of-contents . ,_) (d "[[toc]]")]
    [`(element (make-style #f (list (url-anchor ,a))) '())
      (d (format "<a name=~s></a>" a))]
    [`(require . ,_) (void)]
    [`(mint-scope ,ms)
      (set-box! mint-scope ms)
      (void)]
    [`(define . ,_) (void)]
    [`(deftech ,c)
      ;; XXX
      (d (format "<Defn :name=\"~a\">~a</Defn>" c c))]
    [`(error ,x) (ego `(section #:tag ,x ,x))]
    [`(mint-define! ,@ts)
      (define s (unbox mint-scope))
      (cond
        [(not s)
         (set-box! BAD #t)
         (eprintf "XXX no mint-scope at ~v\n" ts)]
        [else
          (for ([ts (in-list ts)])
            (match-define `'(,@tsl) ts)
            (define t (apply string-append tsl))
            ;; XXX
            (d (format "<Ref :name=\"~a:~a\" />" s t)))])]
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
     ;; XXX link
      (d (format "@[code{~a-~a}](@reach-lang/examples/~a)" from to f))]
    [`(reachex #:dir "rpc-client" py-impl 'only ,from ,to ,_)
      ;; XXX link
      (d (format "@[code{~a-~a}](@reach-lang/rpc-client/py/src/reach_rpc/__init__.py)" from to))]
    [(or
       `(reachex ,f)
       `(reachex #:mode ,_ ,f))
     ;; XXX link
      (d (format "@[code](@reach-lang/examples/~a)" f))]
    ['reach-vers
     (d "{{ VERSION }}")]
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

(define (go! sp dest)
  (match-define
    `(module ,_ ,_ (#%module-begin doc ,@cs))
    (syntax->datum (read-lang-file sp)))
  (with-output-to-file dest
    #:exists 'replace
    (lambda ()
      (egol cs))))

(define BAD (box #f))
(define mint-scope (box #f))
(module+ main
  (define ns (normalize-path scrbl))
  (for ([p (in-directory scrbl)])
    (define bn (file-name-from-path p))
    (when (equal? #".scrbl" (path-get-extension bn))
      (set-box! BAD #f)
      (set-box! mint-scope #f)
      (define rp (find-relative-path ns (normalize-path p)))
      (define n (build-path "src" (path-replace-extension rp #".md")))
      (make-parent-directory* n)
      (go! p n)
      (when (unbox BAD)
        (eprintf "^ >>> ~a\n" n)))))
