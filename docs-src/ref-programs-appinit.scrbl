#lang scribble/manual
@(require scribble/bnf
          "lib.rkt")
@(mint-scope 'rsh)

@title[#:version reach-vers #:tag "ref-programs-appinit"]{Application Initialization}

The body of @reachin{Reach.app} is an @deftech{application initialization}.
It defines the various @tech{participants} and @tech{views} of the @|DApp|, as well as sets @tech{compilation options}.
It is finalized with @reachin{deploy()} and then the application begins in a @tech{step}.

@section[#:tag "ref-programs-appinit-stmts"]{Statements}

Any statements valid for a @seclink["ref-programs-compute-stmts"]{computation} are valid for application initialization.
However, some additional statements are allowed.

@subsection{@tt{deploy}}

@(mint-define! '("deploy"))
@reach{
  deploy(); }

A @deftech{deploy statement}, written @reachin{deploy();}, @tech{deploys} the @|DApp| and finalizes all of the available @tech{participants}, @tech{views}, and @tech{compilation options}.

Its continuation is a @tech{step}, which means its content is specified by @secref["ref-programs-step"].
It represents the body of the @|DApp| to be @tech{compile}d.

@subsection{@tt{setOptions}}

@(mint-define! '("setOptions"))
@reach{
  setOptions({ verifyArithmetic: true });
  setOptions({}); }

The @deftech{compilation options} for the @|DApp| may be set by calling @reachin{setOptions(OBJ_EXPR);} where @reachin{OBJ_EXPR} is an object with the following keys and values:

@itemlist[

@item{
  @(mint-define! '("deployMode")) @reachin{deployMode}

  @reachin{'constructor'} (default) or @reachin{'firstMsg'}

  Determines whether @tech{contract} should be @tech{deploy}ed independently (@reachin{'constructor'}) or as part of the first @tech{publication} (@reachin{'firstMsg'}).
  If deployed as part of the first publication, then the first publication must precede all uses of @reachin{wait} and @reachin{.timeout}.
  See @seclink["guide-deploymode"]{the guide on deployment modes} for a discussion of why to choose a particular mode.
}

@item{
  @(mint-define! '("verifyArithmetic")) @reachin{verifyArithmetic}

  @reachin{true} or @reachin{false} (default)

  Determines whether arithmetic operations automatically introduce static assertions that they do not overflow beyond @reachin{UInt.max}.
  This defaults to @reachin{false}, because it is onerous to verify.
  We recommend turning it on before final deployment, but leaving it off during development.
  When it is @reachin{false}, @tech{connectors} will ensure that overflows do not actually occur on the network.
}

@item{
  @(mint-define! '("verifyPerConnector")) @reachin{verifyPerConnector}

  @reachin{true} or @reachin{false} (default)

  Determines whether verification is done per connector, or once for a generic connector.
  When this is @reachin{true}, then connector-specific constants, like @reachin{UInt.max}, will be instantiated to literal numbers.
  This concretization of these constants can induce performance degradation in the verifier.
}

@item{
  @(mint-define! '("connectors")) @reachin{connectors}

  @(mint-define! '("ETH") '("ALGO")) @reachin{[ETH, ALGO]} (default)

  A tuple of the @tech{connectors} that the application should be compiled for.
  By default, all available @tech{connectors} are chosen.
}

]

@section[#:tag "ref-programs-appinit-exprs"]{Expressions}

Any expressions valid for a @seclink["ref-programs-appinit-exprs"]{computation} are valid for application initialization.
However, some additional expressions are allowed.

@subsection{Participant Definition}

A @tech{participant} and @tech{participant class} may be declared with

@(mint-define! '("Participant"))
@reach{
  Participant(participantName, participantInteractInterface)}

and

@(mint-define! '("ParticipantClass"))
@reach{
  ParticipantClass(participantName, participantInteractInterface)}

respectively.

@reachin{participantName} is a string which indicates the name of the @tech{participant} function in the generated @tech{backend} code.
Each @reachin{participantName} must be unique.

@reachin{participantInteractInterface} is a @deftech{participant interact interface}, an object where each field indicates the type of a function or value which must be provided to the @tech{backend} by the @tech{frontend} for @tech{interact}ing with the @tech{participant}.

@subsection[#:tag "ref-programs-appinit-view"]{View Definition}

@margin-note{This section is about defining @tech{views} during in application initialization. Views are @seclink["ref-programs-consensus-view"]{set in consensus steps}, in your Reach program. But, they are @seclink["ref-frontends-js-view"]{accessed by frontends} by using the Reach standard library of the frontend language, such as JavaScript.}

@(mint-define! '("View"))
@reach{
  View('NFT', { owner: Address })
}

A @tech{view} is defined with @reachin{View(viewName, viewInterface)}, where @reachin{viewName} is a string that labels the @tech{view} and @reachin{viewInterface} is an object where each field indicates the type of a function or value provided by the @tech{contract} associated with the specified @|DApp|.
These @tech{views} are available in @tech{frontends} via the @jsin{ctc.getViews} function.
In the @|DApp|, the result of this application argument is referred to as a @tech{view object}.

