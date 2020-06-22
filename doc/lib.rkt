#lang at-exp racket/base
(require scribble/manual
         scribble/minted)
(provide (all-defined-out))

(define (reach . contents)
  (apply minted "javascript" contents))

(define DApp @tech{DApp})
(define DApps @tech{DApps})
