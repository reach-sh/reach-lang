#lang at-exp racket/base
(require scribble/manual
         scribble/minted
         scriblib/figure
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

(define-runtime-path x "x")

(define (reachexlink p [label #f])
  (define url
    (format "https://github.com/reach-sh/reach-lang/blob/master/docs-src/x/~a" p))
  @link[url (or label url)])

(define (reachex #:show-lines? [show-lines? #f] path . which)
  (define input (file->lines (build-path x path)))
  (define-values (num-pad ls)
    (cond [(not show-lines?) (values "" input)]
          [else
           (define line-buffer "    ")
           (define max-line-w
             (string-length
              (number->string
               (string-length (argmax string-length input)))))
           (values
            (string-append (make-string max-line-w #\.)
                           line-buffer)
            (for/list ([e (in-list input)]
                       [i (in-naturals 1)])
              (string-append (~r #:min-width max-line-w i)
                             line-buffer
                             e)))]))
  (define sel
    (match which
      ['() ls]
      [(list 'skip from to skip-s)
       (define once? #f)
       (filter
        (λ (x) x)
        (for/list ([e (in-list ls)]
                   [i (in-naturals 1)])
          (if (and (<= from i) (<= i to))
            (if once? #f
                (begin (set! once? #t)
                       (string-append num-pad skip-s)))
            e)))]
      [(list 'only from to skip-s)
       (define once? #f)
       (filter
        (λ (x) x)
        (for/list ([e (in-list ls)]
                   [i (in-naturals 1)])
          (if (and (<= from i) (<= i to))
            (begin (set! once? #f) e)
            (if once? #f
                (begin (set! once? #t)
                       (string-append num-pad skip-s))))))]
      [(list from) (drop ls from)]
      [(list #f to) (take ls to)]
      [(list from to)
       (drop (reverse (drop (reverse ls) to)) from)]))
  (apply reach (add-between sel "\n")))
