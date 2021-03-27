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

;; (define-runtime-path analytics-p "analytics.html")
;; (define analytics (file->string analytics-p))
;; (define analytics-head (cdata #f #f analytics))
;; (define (insert x l)
;;   (if (member x l) l (cons x l)))
;; (current-html-render-head-prefix
;;  (insert analytics-head (current-html-render-head-prefix)))

(define-runtime-path VERSION "../VERSION")
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

(define (experimental)
  @margin-note{This section describes an experimental feature of Reach.})

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
@cmd{mkdir -p ~/reach/@|WHICH|}

And that you have a copy of Reach @seclink["install"]{installed} in @tt{~/reach} so you can write
@cmd{../reach version}

And it will run Reach.

You should start off by initializing your Reach program:
@cmd{../reach init}})

(define (the-community-link)
  @link["https://discord.gg/AZsgcXu"]{the Discord community})

(define (cmd . args)
  (apply commandline @bold{@exec{$}} " " args))

(define-runtime-path x "../examples/")
(define-runtime-path rpc-client "../rpc-client/")
(define-runtime-path images "./images/")

(define (eximage path)
  (image (build-path images path)
         #:scale 0.6))

(define (exviewfig-name dir view)
  (format "fig:~a/views/~a" dir view))

;; The following should be true:
;; * examples/${dir}/views/${src-file}.js exists
;; * ${view} is defined from ${src-from} to ${src-to} in that file
;; * docs-src/${dir}/${view}.png exists
(define (exviewfig dir view src-file src-from src-to)
  (define link-file (format "~a.js" src-file))
  (define link
    (reachexlink
      (format "~a/views/~a" dir (format "~a.js" src-file))
      (format "~a.~a" src-file view)
      #:loc (cons src-from src-to)))
  (define caption
    (list "The " @jsin{@view} " view. See: " link))
  @figure[(exviewfig-name dir view) caption]{
    @eximage[(build-path dir (format "~a.png" view))]})

;; dir must be present in examples/ for the links to work
(define (exviewfigs dir src-file . views)
  (for/list ([view-info views])
    (match view-info
      [(list view src-from src-to)
       @exviewfig[dir view src-file src-from src-to]])))

(define (exviewref dir view [the "the"])
  (define fig (figure-ref (exviewfig-name dir view)))
  (list the " " @jsin{@view} " view (" fig ")"))

;; Same as exviewref but with capitalized "The"
(define (Exviewref dir view)
  (exviewref dir view "The"))

(define (exloc . ps)
  (number->string
   (for/sum ([p (in-list ps)])
     (match p
       [(? number?) p]
       [_
        (length (file->lines (build-path x p)))]))))

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
         #:link [link? #f]
         #:dir [dir "examples"]
         #:show-lines? [show-lines? #f]
         path . which)

  (define x-dir
    (match dir
      ["examples" x]
      ["rpc-client" rpc-client]
      (error 'runtime-path "~a is not a declared runtime path" dir)))
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
      [(list 'skip from to skip-s)
       (define once? #f)
       (define uz
         (unzip
          (filter
           (λ (x) x)
           (for/list ([e (in-list input)]
                      [i (in-naturals 1)])
             (if (and (<= from i) (<= i to))
               (if once? #f
                   (begin (set! once? #t)
                          (cons skip-s
                                (string-append num-pad skip-s))))
               (cons e (add-num i e)))))))
       (values #f (car uz) (cdr uz))]
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
                        (string-append num-pad (do-trim skip-s))))))))]
      [(list from)
       (values
        from
        (drop input from)
        (drop (add-nums input) from))]
      [(list #f to)
       (values
        #f
        (take input to)
        (take (add-nums input) to))]
      [(list from to)
       (define (f x)
         (drop (reverse (drop (reverse x) to)) from))
       (values
        (cons from to)
        (f input)
        (f (add-nums input)))]))
  (maybe-link link-loc (apply mode (add-between sel "\n"))))

(define (number->nice-string n)
  (define fullr
    (append*
     (for/list ([c (in-list (reverse (string->list (number->string n))))]
                [i (in-naturals)])
       (if (and (zero? (modulo i 3)) (not (zero? i)))
         (list #\, c)
         (list c)))))
  (define len (length fullr))
  (define before 30)
  (define after 32)
  (if (< len (+ before after))
    (list->string
     (reverse fullr))
    (list->string
     (append (take (reverse fullr) before)
             (string->list
              (format "...~a digits..."
                      (- len before after)))
             (reverse (take fullr after))))))
(module+ test
  (number->nice-string 123456)
  (number->nice-string (expt 2 (* 256 3))))



