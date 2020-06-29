#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-programs"]{Reach Programs}

This document describes the structure and content of Reach programs,
including their syntactic forms and the Reach standard library.

@section{Source Files}

Reach @deftech{source files} are either @tech{executables} or @tech{libraries}.

Reach @deftech{executables} start with @litchar{'reach @|reach-short-vers| exe';} and are followed by a sequence of @tech{participant definitions} and @tech{identifier definitions} and a @tech{main function}.

Reach @deftech{libraries} start with @litchar{'reach @|reach-short-vers| lib';} and are followed by a sequence of @tech{identifier definitions}.

@section{Syntactic Forms}

XXX

@section{Standard Library}

XXX

