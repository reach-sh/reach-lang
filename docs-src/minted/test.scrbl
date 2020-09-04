#lang scribble/base
@(require scribble/html-properties
          scribble/core
          racket/runtime-path
          racket/file
          (only-in xml cdata)
          "main.rkt")
@(define-runtime-path here ".")

@(define top #<<END
   <link rel="stylesheet" type="text/css" href="../minted.css" title="default">
END
   )

@title[#:style (style #f (list (head-extra (cdata #f #f top))))]{Example}

Another short example: @(mint "reach" #:inline? #t #:defines '(("=" ">")) "=>") is an arrow expression.

Big example:

@(mint "reach"
       #:defines '(("isHand") ("winner"))
       #:scope "tut-7/index.rsh"
       (file->string
        (build-path here "../../examples/tut-7/index.rsh")))

Big example two:

@(mint "reach"
       #:scope "tut-7/index.rsh"
       (file->string
        (build-path here "../../examples/tut-7/index.rsh")))

Short example:

This is some inline text that references some terms @(mint "reach" #:inline? #t #:scope "tut-7/index.rsh" "isHand") and @(mint "reach" #:inline? #t #:scope "tut-7/index.rsh" "winner") and @(mint "reach" #:inline? #t #:scope "tut-7/index.rsh" "isOutcome").
