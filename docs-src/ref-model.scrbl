#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-model"]{Language Model}

This document describes the fundamental assumptions and concepts of Reach. First, we discuss the model of running a Reach program in @secref["ref-model-eval"]. Next, we discuss the details about compilation of a Reach program that are relevant to Reach programmers in @secref["ref-model-compile"]. Finally, we discuss how Reach programs are syntactically constructed in @secref["ref-model-syntax"].

@section[#:tag "ref-model-eval"]{Evaluation Model}

Reach programs specify a decentralized application (@deftech{DApp}), which is a distributed computation involving many @tech{participants} and utilizing one @tech{contract} on one @tech{consensus network} for reaching agreement on the intermediate @tech{values} of the computation. @margin-note{"Many" is a technical term that means "zero or more".} When the computation terminates, all @tech{participants} agree on the outcome, because they agreed on the intermediate @tech{values}.

A @deftech{consensus network} is a network protocol with a @tech{network token}, a set of @tech{accounts}, a set of @tech{contracts}, and a @tech{time}.
A @deftech{network token} is an opaque unit of account.
A @tech{consensus network}'s @deftech{time} is some monotonically increasing discrete value from a totally ordered set.
A @deftech{time delta} represents the difference between two points in @tech{time} as a discrete number of @tech{time} units.
@tech{Consensus networks} support @deftech{transfers} of @tech{network tokens} between @tech{accounts}. An @deftech{account} is a unique identity (called an @deftech{address}) with a non-negative balance of @tech{network tokens}. @tech{Accounts} may sign @tech{values} in a way that may not be repudiated or impersonated; this is called @deftech{publication}. @deftech{Contracts} are @tech{accounts} with three extra capacities: they persistently store @tech{values} (called the @deftech{consensus state}), they may receive @tech{publications}, and when they receive @tech{publications}, they systematically process them and may modify their @tech{consensus state}, make @tech{publications}, and may @tech{transfer} @tech{network tokens} in response to the reception. The chapter, @secref["ref-networks"], discusses which @tech{consensus networks} are supported by Reach. @margin-note{This description of @tech{consensus networks} is an abstraction that may not be directly implemented by actual networks. For example, in UTXO-based networks, there is not typically an explicitly represented @tech{account} balance ledger. However, such networks do @emph{abstractly} have @tech{accounts} with balances, because particular private keys represent @tech{accounts} which have exclusive access to some set of @tech{network tokens} which is their balance.}

A @deftech{participant} is a logical actor which takes part in a @|DApp|. It is associated with an @tech{account} on the @tech{consensus network}. @margin-note{The same @tech{account} may be used by multiple @tech{participants} in a @|DApp|.} A @tech{participant} has persistently stored @tech{values}, called its @deftech{local state}. It has a @tech{frontend} which it @tech{interacts} with. A @deftech{frontend} is an abstract actor which supports a set of functions which consume and produce @tech{values}; when a @tech{participant} invokes one of these functions it is referred to as @deftech{interact}ion.

Since @DApps have an associated @tech{contract}, they have an associated @tech{account}. @margin-note{The @tech{contract} account must be distinct from all @tech{participant} @tech{accounts}.} This @tech{account} is assumed to be empty when the computation starts.@margin-note{On some @tech{consensus networks}, it is possible for @tech{transfers} to a @tech{contract} @tech{account} to occur outside of the purview of Reach. If this occurs, then those @tech{network tokens} are remitted to the @tech{originator} of the final @tech{consensus transfer}.} Any @tech{network token}s transferred into the @tech{account} must be removed by the @|DApp|'s completion. This is called the @deftech{token linearity property}.

A @|DApp| computation can be seen as a graph of @tech{steps} with a unique first @tech{step}. A @deftech{step} is a set of @tech{local steps} by @tech{participants} followed by a single @tech{consensus step} introduced via a single @tech{consensus transfer}.

A @deftech{local step} is executed by a single @tech{participant} and is a sequence of @tech{local computations}. A @deftech{local computation} may bind a piece of @tech{local state}, @tech{assert} a property of the @tech{local state}, or @tech{interact} with the @tech{frontend}. A @deftech{consensus transfer} is executed by a single @tech{participant} (called the @deftech{originator}) which makes a @tech{publication} of a set of @tech{public} @tech{values} from its @tech{local state} and @tech{transfers} zero or more @tech{network tokens} to the @tech{contract} @tech{account}. A @tech{consensus transfer} specifies an alternative @tech{step}, called a @deftech{timeout}, that is executed if the @tech{originator} fails to make the transfer before a given @tech{time delta} has elapsed. All @tech{local state} is initially @deftech{private}, until it is explicitly made @deftech{public} via a @deftech{declassification}, which is a kind of @tech{local computation}.

A @deftech{consensus step} is a graph of @tech{consensus computations} with a unique first computation.
A @deftech{consensus computation} either binds @tech{consensus state}, @tech{asserts} a property of the @tech{consensus state}, performs a @tech{transfer}, selects between different next @tech{consensus computations}, or @deftech{commits} to the next @tech{step}.

An @deftech{assert}ion is either: a @deftech{knowledge assertion}, which is a claim that one @tech{honest} @tech{participant} cannot know something that another @tech{honest} @tech{participant} does know; a @deftech{static assertion}, which is an always-true formula; an @deftech{assumption}, which is a true formula if @tech{frontends} behave @tech{honest}ly; a @deftech{requirement}, which is a true formula if @tech{participants} behave @tech{honest}ly; or, a @deftech{possibility assertion}, which is a formula for which there exists some values that @tech{honest} @tech{participants} and @tech{frontends} could submit which results in the truth of the formula.
An @deftech{honest} @tech{participant} is one that executes the @tech{steps} specified by the @|DApp|, while an @tech{honest} @tech{frontend} is one that only returns @tech{values} which ensure that all @tech{assumptions} evaluate to the boolean @reachin{true}.

A @deftech{value} is either: the @litchar{null} value, a boolean, an unsigned integer of 256 bits, a string of bytes, an @tech{address}, a fixed tuple of @tech{values}, a statically-sized homogeneous array of @tech{values}, or a fixed record of @tech{values}.
@tech{Values} may be @deftech{digest}ed to produce a @link["https://en.wikipedia.org/wiki/Cryptographic_hash_function"]{cryptographic hash} of their binary encoding.

@tech{Values} are in one of three possible conditions. They could be @tech{consensus state}, in which case they are known to all @tech{participants}. They could be @tech{local state} of a single @tech{participant}, which means they are known by only that @tech{participant}. @tech{Local state} is further divided into @tech{private} @tech{local state}, which cannot be included in a @tech{publication}, and @tech{public} @tech{local state}, which can. These conditions are summarized in @figure-ref["fig:value-states"].

@figure["fig:value-states" "The three conditions of values"]{
@(let ()
(local-require pict)
(define (add-edge all from label to)
  (pin-arrow-line
   10 all
   from rc-find
   to lc-find
   #:line-width 3
   #:label (inset (text label) 5)))

(define (state ls)
  (frame (inset (apply vc-append (map text ls)) 5)))

(define p
  (let* ([initial (blank)]
         [local-private (state '("local" "private"))]
         [local-public (state '("local" "public"))]
         [consensus (state '("consensus"))]
         [p (hc-append 75
                       initial local-private
                       local-public consensus)]
         [p (add-edge p initial "" local-private)]
         [p (add-edge p local-private "declassify" local-public)]
         [p (add-edge p local-public "publish" consensus)]
         [p (inset p 25)])
    p))

p)}

@section[#:tag "ref-model-compile"]{Compilation Model}

Reach programs cannot execute independently of a @tech{consensus network} and a set of @tech{frontends}.
Thus, the semantics of Reach treats these components abstractly and does not specify their semantics.
Therefore, the semantics of Reach cannot be effectively implemented directly in a virtual machine or interpreter.
Instead, Reach programs are @deftech{compile}d to
a particular @tech{consensus network} @deftech{connector}
and a set of @tech{participant} @deftech{backends}
which execute the computation of the particular @tech{consensus network}.
@tech{Connectors} and @tech{backends} are sound
if they faithfully model the abstract semantics assumed by Reach.

During compilation, the Reach compiler automatically verifies that the @tech{token linearity property} and all @tech{static assertions} and @tech{possibility assertions} are true whether @tech{participants} and @tech{frontends} are @tech{honest} or not.
Similarly, all @tech{knowledge assertions} are verified using a conservative approximation of @tech{participant} knowledge.
This conservative approximation assumes that all inputs to a computation are revealed by the result of the computation, except for @tech{digests} and @tech{interact}ion.
This approximation means that Reach cannot, for example, reason about the details of manually expressed encryption formulas and will assume they are insecure.

If these @tech{assert}ions cannot be statically verified, then the compilation process aborts.
After this verification, such @tech{static assertions} and @tech{possibility assertions} are removed from the program and do not occur at runtime.
In contrast, @tech{assumptions} are enforced at runtime by @tech{backends} and @tech{requirements} are enforced at runtime by @tech{connectors}.
If @tech{assumptions} are violated at runtime, then the @tech{backend} aborts.
If @tech{requirements} are violated at runtime, then the @tech{connector} ensures that all aspects of the @|DApp| (the @tech{contract} and @tech{participant}) ignore the inducing @tech{consensus transfer}, which often results in a @tech{timeout}.

@section[#:tag "ref-model-syntax"]{Syntax Model}

Reach programs are specified via a subset of well-formed JavaScript syntax inside @tech{source files}. The section @secref["ref-programs"] describes the syntax of Reach programs in detail.

