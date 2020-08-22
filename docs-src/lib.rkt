#lang at-exp racket/base
(require scribble/manual
         scribble/minted
         scriblib/figure
         scribble/core
         racket/match
         racket/format
         racket/list
         racket/file
         racket/runtime-path)
(provide (all-defined-out)
         (all-from-out scriblib/figure))

(define reach-short-vers "0.1")
(define reach-vers "0.1.0")

(current-pygmentize-default-style 'solarizedlight)
(define (reach . contents)
  (apply minted "javascript" contents))
(define (reachin . contents)
  (apply mintinline "javascript" contents))

(define (js . contents)
  (apply minted "javascript" contents))
(define (jsin . contents)
  (apply mintinline "javascript" contents))

(define DApp @tech{DApp})
(define DApps @tech{DApps})

(define (experimental)
  @margin-note{This section describes an experimental feature of Reach.})

(define (hrule)
  @para{@bold{---}})

(define (the-community-link)
  @link["https://discord.com/channels/628402598663290882"]{the Discord community})

(define (cmd . args)
  (apply commandline @bold{@exec{$}} " " args))

(define-runtime-path x "x")

(define (reachexlink p [label #f])
  (define url
    (format "https://github.com/reach-sh/reach-lang/blob/master/docs-src/x/~a" p))
  @link[url (or label url)])

(define (reachex
         #:mode [mode reach]
         #:link [link? #f]
         #:show-lines? [show-lines? #f]
         path . which)

  (define ((do-link lab) content)
    (list
     (tabular
      #:style 'boxed
      (list (list (reachexlink path (exec lab)))))
     content))
  (define maybe-link
    (match link?
      [#f (λ (x) x)]
      [#t (do-link path)]
      [lab (do-link lab)]))
  
  (define input (file->lines (build-path x path)))
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
  (define sel
    (match which
      ['() (add-nums input)]
      [(list 'skip from to skip-s)
       (define once? #f)
       (filter
        (λ (x) x)
        (for/list ([e (in-list input)]
                   [i (in-naturals 1)])
          (if (and (<= from i) (<= i to))
            (if once? #f
                (begin (set! once? #t)
                       (string-append num-pad skip-s)))
            (add-num i e))))]
      [(list 'only from to skip-s)
       (define trim-amt
         (let loop ([l (string->list skip-s)])
           (match l
             ['() 0]
             [(cons #\space l) (add1 (loop l))]
             [_ 0])))
       (define (do-trim s)
         (define-values (spaces others)
           (split-at (string->list s) trim-amt))
         (unless (andmap (λ (x) (char=? #\space x)) spaces)
           (error 'do-trim "~v" (vector path which s)))
         (list->string others))
       (define once? #f)
       (filter
        (λ (x) x)
        (for/list ([e (in-list input)]
                   [i (in-naturals 1)])
          (if (and (<= from i) (<= i to))
            (begin (set! once? #f) (add-num i (do-trim e)))
            (if once? #f
                (begin (set! once? #t)
                       (string-append num-pad (do-trim skip-s)))))))]
      [(list from) (drop (add-nums input) from)]
      [(list #f to) (take (add-nums input) to)]
      [(list from to)
       (drop (reverse (drop (reverse (add-nums input)) to)) from)]))
  (maybe-link (apply mode (add-between sel "\n"))))
