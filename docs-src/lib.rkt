#lang at-exp racket/base
(require scribble/manual
         scribble/minted)
(provide (all-defined-out))

(define reach-short-vers "0.1")
(define reach-vers "0.1.0")

(define (reach . contents)
  (apply minted "javascript" contents))

(define DApp @tech{DApp})
(define DApps @tech{DApps})

(define (experimental)
  @margin-note{This section describes an experimental feature of Reach.})
