#lang at-exp racket/base
(require scribble/manual
         scriblib/figure
         scribble/core
         scribble/html-properties
         scribble/html-render
         (only-in xml cdata)
         racket/match
         racket/format
         racket/list
         racket/file
         racket/string
         racket/runtime-path
         "./minted/main.rkt")
(provide (all-defined-out)
         (all-from-out scriblib/figure)
         mint-scope mint-define!)

(define-runtime-path ROOT "../..")

(define VERSION (build-path ROOT "VERSION"))
(define version-ht
  (for/fold ([ht (hasheq)]) ([l (in-list (file->lines VERSION))])
    (match (string-split l "=")
      [(list key val) (hash-set ht (string->symbol key) val)]
      ["" (void)]
      [_ (eprintf "VERSION: skipped ~v\n" l) ht])))
(define (v k)
  (hash-ref version-ht k
            (λ () (error 'version "~a not in ~v" k version-ht))))

(define reach-vers
  (string-join (list (v 'MAJOR) (v 'MINOR) (v 'PATCH)) "."))
(define reach-short-vers
  (string-join (list (v 'MAJOR) (v 'MINOR)) "."))

(define (make-mints lang)
  (define (reach . contents)
    (apply mint lang contents))
  (define (reachin . contents)
    (apply mint #:inline? #t lang contents))
  (values reach reachin))

(define-values (reach reachin) (make-mints "reach"))
(define-values (js jsin) (make-mints "javascript"))
(define-values (py pyin) (make-mints "python"))
(define-values (go goin) (make-mints "go"))
(define-values (yaml _yamlin) (make-mints "yaml"))
(define-values (makefile _makefilein) (make-mints "makefile"))
(define-values (shell _shellin) (make-mints "shell"))

(define (make-tt-cat lab)
  (define (tag x) (list lab x))
  (define sty 'tt)
  (define (ref x)
    (link-element sty x (tag x)))
  (define (def x)
    (toc-target2-element #f (as-index (element sty x))
      (tag x) (tt x)))
  (values def ref))

(define-values (defconmode conmode) (make-tt-cat 'conmode))
(define-values (defenv envref) (make-tt-cat 'env))

(define DApp @tech{DApp})
(define DApps @tech{DApps})
(define RPS @emph{Rock, Paper, Scissors!})

(define (hrule)
  @para{@bold{---}})

(define (check ans . body)
  @margin-note{Check your understanding: @(apply list body) @(nested #:style (style "check-answer" empty) @list{Answer: @(para #:style (style "check-answer-body" empty) ans)})})
(define (check:tf ans . body)
  @check[ans]{True or false: @(apply list body)})
(define (check:multi ans q . opts)
  (define (ensure-item x)
    (if (item? x) x
      @item{@|x|}))
  (define optsp (map ensure-item opts))
  @check[ans]{@|q| @(apply itemlist #:style 'ordered optsp)})
(define (check:many ans q . opts)
  (apply check:multi ans q opts))

(define (many-break n)
  (make-list n @(linebreak)))
(define (stop . args)
  (apply centered
         (many-break 3) @bold{Stop!} @(linebreak)
         (append args (many-break 4))))

(define (drstep-pr t)
  @section[#:tag (format "~a-pr" t)]{Problem Analysis})
(define (drstep-dd t)
  @section[#:tag (format "~a-dd" t)]{Data Definition})
(define (drstep-dd-datatype-mn)
  @margin-note{Refer to @secref["ref-programs-types"] for a reminder of what data types are available in Reach.})
(define (drstep-cc t)
  @section[#:tag (format "~a-cc" t)]{Communication Construction})
(define (drstep-ai t)
  @section[#:tag (format "~a-ai" t)]{Assertion Insertion})
(define (drstep-ii t)
  @section[#:tag (format "~a-ii" t)]{Interaction Introduction})
(define (drstep-de t)
  @section[#:tag (format "~a-de" t)]{Deployment Decisions})

(define (drstep-pr-stop)
  @stop{Write down the problem analysis of this program as a comment.})
(define (drstep-dd-stop)
  @stop{Write down the data definitions for this program as definitions.})
(define (drstep-cc-stop1)
  @stop{Write down the communication pattern for this program as comments.})
(define (drstep-cc-stop2)
  @stop{Write down the communication pattern for this program as code.})
(define (drstep-ai-stop1)
  @stop{Write down the properties you know are true about the various values in the program.})
(define (drstep-ai-stop2)
  @stop{Insert assertions into the program corresponding to facts that should be true.})
(define (drstep-ii-stop)
  @stop{Insert @reachin{interact} calls to the @tech{frontend} into the program.})
(define (drstep-de-stop)
  @stop{Decide how you will deploy and use this application.})

(define (workshop-deps . deps)
  (match deps
    [(list)
     @margin-note{This workshop is independent of all others.}]
    [(list x)
     @margin-note{This workshop assumes that you have recently completed @secref[x].}]))

(define (workshop-init WHICH)
 @list{
We assume that you'll go through this workshop in a directory named @tt{~/reach/@|WHICH|}:
@cmd{mkdir -p ~/reach/@|WHICH| && cd ~/reach/@|WHICH|}

And that you have a copy of Reach @seclink["ref-install"]{installed} in @tt{~/reach} so you can write
@cmd{../reach version}

And it will run Reach.

You should start off by initializing your Reach program:
@cmd{../reach init}})

(define (the-community-link)
  @link["https://discord.gg/AZsgcXu"]{the Discord community})

(define (cmd . args)
  (apply commandline @bold{@exec{$}} " " args))

(define x (build-path ROOT "examples"))
(define rpc-client (build-path ROOT "rpc-client"))
(define-runtime-path images "./images/")

(define (reachexlink p [label #f] #:loc [loc #f] #:dir [dir "examples"])
  (define url
    (format "https://github.com/reach-sh/reach-lang/blob/master/~a/~a~a"
            dir
            p
            (match loc
              [#f ""]
              [(cons from to)
               (format "#L~a-L~a" from to)]
              [from
               (format "#L~a" from)])))
  @link[url (or label (exec p))])

(define (reachex
         #:mode [mode reach]
         #:dir [dir "examples"]
         path . which)
  (define link? #t)
  (define show-lines? #t)

  (define x-dir
    (match dir
      ["examples" x]
      ["rpc-client" rpc-client]
      [_ (error 'runtime-path "~a is not a declared runtime path" dir)]))
  (define ((do-link lab) link-loc content)
    (define copy
      (cond
        [copyt
         (define raw
           `(button ([class "btn"] [data-clipboard-text ,(string-join copyt "\n")])
                    (img ([class "clippy"] [width "13"] [src "clippy.svg"] [alt "Copy to clipboard"]) " ")))
         (elem #:style (style #f (list (xexpr-property raw 'nbsp))) " ")]
        [else
         ""]))
    (list
     (tabular
      #:style 'boxed
      (list (list (para (reachexlink path (exec lab) #:loc link-loc #:dir dir) copy))))
     content))
  (define maybe-link
    (match link?
      [#f (λ (_ x) x)]
      [#t (do-link path)]
      [lab (do-link lab)]))

  (define input (file->lines (build-path x-dir path)))
  (define-values (num-pad add-num)
    (cond [(not show-lines?)
           (values "" (λ (i e) e))]
          [else
           (define line-buffer "    ")
           (define max-line-w
             (string-length
              (number->string
               (string-length (argmax string-length input)))))
           (values
            (string-append (make-string max-line-w #\.)
                           line-buffer)
            (λ (i e)
              (string-append (~r #:min-width max-line-w i)
                             line-buffer
                             e)))]))
  (define (add-nums x)
    (for/list ([e (in-list x)]
               [i (in-naturals 1)])
      (add-num i e)))
  (define (unzip l) (cons (map car l) (map cdr l)))
  (define-values (link-loc copyt sel)
    (match which
      ['()
       (values
        #f input
        (add-nums input))]
      [(list 'only from to skip-s)
       (define trim-amt
         (let loop ([l (string->list skip-s)])
           (match l
             ['() 0]
             [(cons #\space l) (add1 (loop l))]
             [_ 0])))
       (define (do-trim s)
         (cond
           [(<= trim-amt (string-length s))
            (define-values (spaces others)
              (split-at (string->list s) trim-amt))
            (cond
              [(andmap (λ (x) (char=? #\space x)) spaces)
               (list->string others)]
              [else
               s])]
           [else
            s]))
       (define once? #f)
       (values
        (cons from to)
        (for/list ([e (in-list input)]
                   [i (in-naturals 1)]
                   #:when (and (<= from i) (<= i to)))
          e)
        (filter
         (λ (x) x)
         (for/list ([e (in-list input)]
                    [i (in-naturals 1)])
           (if (and (<= from i) (<= i to))
             (begin (set! once? #f) (add-num i (do-trim e)))
             (if once? #f
                 (begin (set! once? #t)
                        (string-append num-pad (do-trim skip-s))))))))]))
  (maybe-link link-loc (apply mode (add-between sel "\n"))))

(define (pkg-fmts)
  (let* ([host-and-acct (lambda (x) (car (string-split x "/" #:trim? #f)))]

         [fmts (for*/list
            ([host '("" "server:")]
             [ref  '("" "#" "#ref")]
             [path '("" ":" ":a/b/file.rsh" ":a/b/" ":file.rsh")])
            (format "@~aaccount/repo~a~a" host ref path))]

         [groups (for/list ([g (group-by host-and-acct fmts)])
                    (string-join g "\n"))])

    @verbatim{@(string-join groups "\n\n")}))

(define (error-version #:from [from #f] #:to [to #f])
 (cond
  [(and from to)
    @margin-note{This error will not happen before version @tt{@from} or after version @tt{@to}}]
  [from
    @margin-note{This error will not happen before version @tt{@from}}]
  [to
    @margin-note{This error will not happen after version @tt{@to}}]))
