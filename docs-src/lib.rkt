#lang at-exp racket/base
(require scribble/manual
         scribble/minted
         scriblib/figure)
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
