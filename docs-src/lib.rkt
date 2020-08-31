#lang at-exp racket/base
(require scribble/manual
         scribble/minted
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
         racket/runtime-path)
(provide (all-defined-out)
         (all-from-out scriblib/figure))

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
      [_ (eprintf "VERSION: skipped ~v\n" l) ht])))
(define (v k)
  (hash-ref version-ht k
            (λ () (error 'version "~a not in ~v" k version-ht))))

(define reach-vers
  (string-join (list (v 'MAJOR) (v 'MINOR) (v 'PATCH)) "."))
(define reach-short-vers
  (string-join (list (v 'MAJOR) (v 'MINOR)) "."))

(current-pygmentize-default-style 'solarizedlight)
(define (reach . contents)
  (apply minted "reach" contents))
(define (reachin . contents)
  (apply mintinline "reach" contents))

(define (js . contents)
  (apply minted "javascript" contents))
(define (jsin . contents)
  (apply mintinline "javascript" contents))

(define (yaml . contents)
  (apply minted "yaml" contents))
(define (makefile . contents)
  (apply minted "makefile" contents))

(define DApp @tech{DApp})
(define DApps @tech{DApps})

(define (experimental)
  @margin-note{This section describes an experimental feature of Reach.})

(define (hrule)
  @para{@bold{---}})

(define (the-community-link)
  @link["https://discord.gg/AZsgcXu"]{the Discord community})

(define (cmd . args)
  (apply commandline @bold{@exec{$}} " " args))

(define-runtime-path x "../examples/")

(define (exloc . ps)
  (number->string
   (for/sum ([p (in-list ps)])
     (match p
       [(? number?) p]
       [_
        (length (file->lines (build-path x p)))]))))

(define (reachexlink p [label #f] #:loc [loc #f])
  (define url
    (format "https://github.com/reach-sh/reach-lang/blob/master/examples/~a~a"
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
         #:show-lines? [show-lines? #f]
         path . which)

  (define ((do-link lab) link-loc content)
    (list
     (tabular
      #:style 'boxed
      (list (list (reachexlink path (exec lab) #:loc link-loc))))
     content))
  (define maybe-link
    (match link?
      [#f (λ (_ x) x)]
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
  (define-values (link-loc sel)
    (match which
      ['()
       (values
        #f
        (add-nums input))]
      [(list 'skip from to skip-s)
       (define once? #f)
       (values
        #f
        (filter
         (λ (x) x)
         (for/list ([e (in-list input)]
                    [i (in-naturals 1)])
           (if (and (<= from i) (<= i to))
             (if once? #f
                 (begin (set! once? #t)
                        (string-append num-pad skip-s)))
             (add-num i e)))))]
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
        (drop (add-nums input) from))]
      [(list #f to)
       (values
        #f
        (take (add-nums input) to))]
      [(list from to)
       (values
        (cons from to)
        (drop (reverse (drop (reverse (add-nums input)) to)) from))]))
  (maybe-link link-loc (apply mode (add-between sel "\n"))))
