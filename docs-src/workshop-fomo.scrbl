#lang scribble/manual
@(require "lib.rkt")

@(define TAG "workshop-fomo")
@title[#:version reach-vers #:tag TAG]{Workshop: FOMO}

In this workshop, we'll design an application that allows a Funder
to create an auction where participants may purchase tickets.
The Funder sets a ticket price and a relative deadline and any time
a Buyer purchases a ticket, the deadline is reset. Whomever is the
last person to buy a ticket—when the deadline finally hits—wins
the entire balance.

@(workshop-deps)
@(workshop-init TAG)

@(drstep-pr TAG)

First, we should think over the details of the application and
answer some questions to help reason about the implementation of
the program.

You should write the answer to the following questions in your
Reach program (@tt{index.rsh}) using a comment.
@reachin{/* Remember comments are written like this. */}

@itemlist[
  @item{Who is involved in this application?}
  @item{What information do they know at the start of the program?}
  @item{What information are they going to discover and use
  in the program?}
  @item{What funds change ownership during the application and how?}
]

@(drstep-pr-stop)

Let's see how your answers compare to ours:

@itemlist[
  @item{This program involves two parties: a Funder who deploys the
    auction and the Buyers who purchase tickets.}
  @item{The Funder knows the deadline and the ticket price at the
  start of the application.}
  @item{The Buyers do not know anything of the deadline nor ticket price when the application begins.}
  @item{The Funder does not learn anything during the program execution.}
  @item{The Buyer learns of the ticket price and deadline during the
  program execution.}
  @item{Buyers continually add funds to the balance during execution until one Buyer wins the entire balance.}
]

It's okay if some of your answers differ from ours!
