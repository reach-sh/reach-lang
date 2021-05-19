#lang scribble/manual
@(require scribble/bnf
          "lib.rkt")

@title[#:version reach-vers #:tag "ref-programs" #:style 'toc]{Programs}

This document describes the structure and content of Reach @deftech{programs}, including
their syntactic forms,
the standard library,
and the standards of @tech{valid} programs.

@margin-note{Get language support for Reach in your editor by visiting @seclink["guide-editor-support"].}

The rest of this section is structured according to the contexts of the different parts of a Reach program, as follows:

@itemlist[

@item{@Secref["ref-programs-valid"] describes what is meant by the term @tech{valid} in Reach.}

@item{@Secref["ref-programs-module"] describes the top-level structure of Reach @tech{module}.}

@item{@Secref["ref-programs-step"] describes the structure of Reach @tech{steps}.}

@item{@Secref["ref-programs-local"] describes the structure of Reach @tech{local steps}.}

@item{@Secref["ref-programs-consensus"] describes the structure of Reach @tech{consensus steps}.}

@item{@Secref["ref-programs-compute"] describes the common structure of Reach computations shared by all contexts.}

]

@Figure-ref["fig:app-steps"] shows the relationship between the modes of a Reach application.

@figure["fig:app-steps" @elem{The modes of a Reach application}
  ]{@image["images/reference/StepDiagram.png" #:style "fig"]}

@section[#:tag "ref-programs-valid"]{Validity}

Reach imposes further restrictions on syntactically well-formed programs.
These restrictions are described throughout this manual using the term @deftech{valid} to refer to constructions that obey the restrictions,
and the term @deftech{invalid} to refer to constructions that do not obey them.

It is always @tech{invalid} to use a @tech{value} with an operation for which it is undefined.
For example, @reachin{1 + true} is @tech{invalid}.
In other words, Reach enforces a static type discipline.

@subsection{Security levels and scope}

The text of Reach program is @tech{public} knowledge to all @tech{participants}.
However, any value that comes from an @tech{interaction expression} is a @deftech{secret} which only that participant knows.
Furthermore, any values derived from @tech{secret} values are also @tech{secret}.
A value, X, is considered derived from another, Y, if the value of Y is provided to a primitive operation to arrive at X, or if Y is used as part of a conditional that influences the definition of X.
@tech{Secrets} can only be made @tech{public} by using the @tech{declassify} primitive.

When @tech{secret} values are bound to an @tech{identifier}
within a @tech{local step},
the identifier name MUST be prefixed by an underscore (@reachin{_}).

When @tech{public} values are bound to an @tech{identifier},
regardless of context,
the identifier name MUST NOT be prefixed by an underscore (@reachin{_}).

Consequently, identifiers which appear inside of a
@tech{function definition} or @tech{arrow expression}
MAY be prefixed by an underscore.
This will cause a compiler error if any value bound to that
identifier is public.

@section[#:tag "ref-programs-module"]{Modules}

A Reach @deftech{source file} is a textual file which specifies a Reach @tech{module}.
It is traditionally given the file extension @litchar{rsh},
e.g. @filepath{dao.rsh}.

A @deftech{module} starts with @reachin{'reach @|reach-short-vers|';}
followed by a sequence of @tech{imports} and @tech{identifier definitions}.
A module can only be compiled or used if it contain one or more @tech{exports}.
@margin-note{See @seclink["guide-versions"]{the guide section on versions} to understand how Reach uses version numbers like this.}

@subsection[#:tag "ref-programs-module-stmts"]{Statements}

Any statements valid for a @seclink["ref-programs-compute-stmts"]{computation} are valid for a module.
However, some additional statements are allowed.

@subsubsection[#:tag "ref-programs-export"]{@tt{export}}

Module-level @tech{identifier definitions} may be @deftech{export}ed
by writing @(mint-define! '("export")) @reachin{export} in front of them.
For example,
@reach{
  export const x = 1;
  export const [a, b, ...more] = [ 0, 1, 2, 3, 4 ];
  export function add1(x) { return x + 1; };
}
are valid @tech{exports}.

Module-level identifiers may also be @tech{export}ed after the fact,
and may be renamed during export. For example:

@reach{
 const w = 2;
 const z = 0;
 export {w, z as zero};
}

Identifiers from other modules may be re-exported (and renamed),
even if they are not imported in the current module.
For example:

@reach{
 export {u, x as other_x} from './other-module.rsh';
}

An @tech{export}ed identifier in a given @tech{module} may be @tech{import}ed by other @tech{modules}.

Exports are also exposed to the frontend via @jsin{getExports}. Functions are only exposed
if they are typed, that is, if they are constructed with @reachin{is}.

@subsubsection[#:tag "ref-programs-import"]{@tt{import}}

@(mint-define! '("import"))
@reach{import 'games-of-chance.rsh';}

When a @tech{module}, @litchar{X}, contains an @deftech{import},
written @reachin{import "LIB.rsh";},
then the path @filepath{LIB.rsh} must resolve to another Reach @tech{source file}.
The @tech{exports} from the @tech{module} defined by @filepath{LIB.rsh} are included in the set of @tech{bound identifier}s in @litchar{X}.

@(mint-define! '("from"))
@reach{import {flipCoin, rollDice as d6} from 'games-of-chance.rsh';}

Import statements may limit or rename the imported @tech{identifiers}.

@reach{import * as gamesOfChance from 'games-of-chance.rsh';}

Imports may instead bind the entire @tech{module} to a single @tech{identifier},
which is an @tech{object} with @tech{fields} corresponding to that @tech{module}'s @tech{exports}.

@tech{Import} cycles are @tech{invalid}.

The path given to an @tech{import} may @bold{not} include @litchar{..} to specify files outside the current directory @bold{nor} may it be an absolute path.

It @bold{must} be a relative path, which is resolved relative to the parent directory of the @tech{source file} in which they appear.

@subsection[#:tag "ref-programs-module-exprs"]{Expressions}

Any expressions valid for a @seclink["ref-programs-compute-exprs"]{computation} are valid for a module.
However, some additional expressions are allowed.

@subsubsection[#:tag "ref-programs-reach.app"]{@tt{Reach.App}}

@(mint-define! '("Reach") '("App"))
@reach{
export const main =
  Reach.App({}, [Participant("A", {displayResult: Fun(Int, Null)})], (A) => {
    const result = 0;
    A.only(() => { interact.displayResult(result); })
    return result;
  });
}

@deftech{Reach.App} is a function which accepts three arguments:
@reachin{options},
@reachin{applicationArgs},
and @reachin{program}.

The @reachin{options} must be an object.
It supports the following options:

@tabular[
#:style 'boxed
(list

(list
 @(begin @(mint-define! '("deployMode")) @reachin{deployMode})
 @~
 @para{@reachin{'constructor'} (default) or @reachin{'firstMsg'}}
 @~
 @para{Determines whether @tech{contract} should be @tech{deploy}ed independently (@reachin{'constructor'}) or as part of the first @tech{publication} (@reachin{'firstMsg'}).
 If deployed as part of the first publication, then the first publication must precede all uses of @reachin{wait} and @reachin{.timeout}.
 See @seclink["guide-deploymode"]{the guide on deployment modes} for a discussion of why to choose a particular mode.}
 )

(list @~ @~ @~ @~ @~)

(list
 @(begin @(mint-define! '("verifyArithmetic")) @reachin{verifyArithmetic})
 @~
 @para{@reachin{true} or @reachin{false} (default)}
 @~
 @para{Determines whether arithmetic operations automatically introduce static assertions that they do not overflow beyond @reachin{UInt.max}.
 This defaults to @reachin{false}, because it is onerous to verify.
 We recommend turning it on before final deployment, but leaving it off during development.
 When it is @reachin{false}, @tech{connectors} will ensure that overflows do not actually occur on the network.}
 )

(list @~ @~ @~ @~ @~)

(list
 @(begin @(mint-define! '("verifyPerConnector")) @reachin{verifyPerConnector})
 @~
 @para{@reachin{true} or @reachin{false} (default)}
 @~
 @para{Determines whether verification is done per connector, or once for a generic connector.
 When this is @reachin{true}, then connector-specific constants, like @reachin{UInt.max}, will be instantiated to literal numbers.
 This concretization of these constants can induce performance degradation in the verifier.}
 )

(list @~ @~ @~ @~ @~)

(list
 @(begin @(mint-define! '("connectors")) @reachin{connectors})
 @~
 @para{@(mint-define! '("ETH") '("ALGO")) @reachin{[ETH, ALGO]} (default)}
 @~
 @para{A tuple of the @tech{connectors} that the application should be compiled for.
 By default, all available @tech{connectors} are chosen.}
 )

)]

The @reachin{applicationArgs} argument is a tuple of @tech{application arguments}.

The @reachin{program} argument must be a syntactic @tech{arrow expression}.
The arguments to this arrow must match the number and order of @reachin{applicationArgs}.
The function body is the program to be @tech{compile}d.
It specifies a @tech{step}, which means its content is specified by @Secref["ref-programs-step"].
When it returns, it must be in a @tech{step}, as well; which means that its content cannot end within a @tech{consensus step}.

If the result of @reachin{Reach.App} is eventually bound to an identifier that is @tech{export}ed, then that identifier may be a target given to the compiler, as discussed in @seclink["ref-usage-compile"]{the section on usage}.

@subsubsection{Application Arguments}

An @deftech{application argument} is used for declaring the components of a Reach @|DApp|.
These components are either @tech{participants} or @tech{views}.

@(hrule)

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

@(hrule)

@(mint-define! '("View"))
@reach{
  View('NFT', { owner: Address })
}

A @tech{view} is defined with @reachin{View(viewName, viewInterface)}, where @reachin{viewName} is a string that labels the @tech{view} and @reachin{viewInterface} is an object where each field indicates the type of a function or value provided by the @tech{contract} associated with the specified @|DApp|.
These @tech{views} are available in @tech{frontends} via the @jsin{ctc.getViews} function.
In the @|DApp|, the result of this application argument is referred to as a @tech{view object}.

@section[#:tag "ref-programs-step"]{Steps}

A Reach @tech{step} occurs in the body of @reachin{Reach.App} or in the @tech{continuation} of a @tech{commit statement}.
It represents the actions taken by each of the participants in an application.

@subsection[#:tag "ref-programs-step-stmts"]{Statements}

Any statements valid for a @seclink["ref-programs-compute-stmts"]{computation} are valid for a step.
However, some additional statements are allowed.

@subsubsection[#:tag "ref-programs-only-step"]{@tt{only} and @tt{each}}

@(mint-define! '("only"))
@reach{
 Alice.only(() => {
   const pretzel = interact.random(); }); }

A @tech{local step} statement is written @reachin{PART.only(() => BLOCK)}, where @reachin{PART} is a @tech{participant} identifier and @reachin{BLOCK} is a @tech{block}.
Within @reachin{BLOCK}, @reachin{PART} is bound to the @tech{address} of the participant.
Any bindings defined within the @tech{block} of a @tech{local step} are available in the @tech{statement}'s @tech{tail} as new @tech{local state}.
For example,

@reach{
 Alice.only(() => {
   const x = 3; });
 Alice.only(() => {
   const y = x + 1; }); }

is a @tech{valid} program where @reachin{Alice}'s @tech{local state} includes the @tech{private} values @reachin{x} (bound to @reachin{3}) and @reachin{y} (bound to @reachin{4}). However, such bindings are @emph{not} @tech{consensus state}, so they are purely @tech{local state}. For example,

@reach{
 Alice.only(() => {
   const x = 3; });
 Bob.only(() => {
   const y = x + 1; }); }

is an @tech{invalid} program, because @reachin{Bob} does not know @reachin{x}.

@(hrule)

@(mint-define! '("each"))
@reach{
 each([Alice, Bob], () => {
   const pretzel = interact.random(); }); }

An @deftech{each} @tech{local step} statement can be written as @reachin{each(PART_TUPLE () => BLOCK)}, where @reachin{PART_TUPLE} is a tuple of @tech{participants} and @reachin{BLOCK} is a @tech{block}.
It is an abbreviation of many @tech{local step} statements that could have been written with @reachin{only}.

@subsubsection{Pay Amounts}

A @deftech{pay amount} is either:
@itemlist[
@item{An integer, denoting an amount of @tech{network tokens}; or,}

@item{A tuple of @tech{token amounts}.}
]

A @deftech{token amount} is either:
@itemlist[
@item{An integer, denoting an amount of @tech{network tokens}; or,}

@item{A tuple with two elements, where the first is an integer, denoting an amount of @tech{non-network tokens}, and the second is @reachin{Token}, specifying a particular @tech{non-network token}.}
]

For example, these are all @tech{pay amounts}:
@reach{
0
5
[ 5 ]
[ 5, [ 2, gil ] ]
[ [ 2, gil ], 5 ]
[ 5, [ 2, gil ], [ 8, zorkmids ] ]
}

It is @tech{invalid} for a @tech{pay amount} to specify an amount of tokens multiple times.
For examples, these are @tech{invalid} @tech{pay amounts}:
@reach{
[ 1, 2 ]
[ [2, gil], [1, gil] ]
}

The ordering of a @tech{pay amount} is only significant when used within a @tech{fork statement} or @tech{parallel reduce statement} that specifies a @reachin{paySpec}.
In this case, payments are expected to be a tuple where the first element is an integer @tech{pay amount}, and the rest of the elements are @tech{token amount} tuples. The ordering of the @tech{token amount} elements should match the ordering in @reachin{paySpec}. For example,
@reach{
  .paySpec([tokA, tokB])}

will indicate that @reachin{fork} payments should be of the format:

@reach{
  [ NETWORK_TOKEN_AMT, [ amtA, tokA ], [ amtB, tokB ] ]}


@subsubsection{@tt{publish}, @tt{pay}, @tt{when}, and @tt{timeout}}

@(mint-define! '("publish") '("pay") '("when") '("timeout"))
@reach{
 Alice.publish(wagerAmount)
      .pay(wagerAmount)
      .timeout(DELAY, () => {
        Bob.publish();
        commit();
        return false; }); }
@reach{
 Alice.publish(wagerAmount)
      .pay(wagerAmount)
      .timeout(DELAY, () => closeTo(Bob, false)); }
@reach{
 Alice.publish(wagerAmount)
      .pay(wagerAmount)
      .timeout(false); }

A @tech{consensus transfer} is written @reachin{PART_EXPR.publish(ID_0, ..., ID_n).pay(PAY_EXPR)..when(WHEN_EXPR).timeout(DELAY_EXPR, () => TIMEOUT_BLOCK)},
where @reachin{PART_EXPR} is an expression that evaluates to a @tech{participant} or @tech{race expression},
@reachin{ID_0} through @reachin{ID_n} are identifiers for @reachin{PART}'s @tech{public} @tech{local state},
@reachin{PAY_EXPR} is a @tech{public} @tech{expression} evaluating to a @tech{pay amount},
@reachin{WHEN_EXPR} is a @tech{public} @tech{expression} evaluating to a boolean and determines if the @tech{consensus transfer} takes place,
@reachin{DELAY_EXPR} is a @tech{public} @tech{expression} that depends on only @tech{consensus state} and evaluates to a @tech{time delta} represented by a natural number,
@reachin{TIMEOUT_BLOCK} is a @tech{timeout} @tech{block}, which will be executed after @reachin{DELAY_EXPR} units of @tech{time} have passed from the end of the last @tech{consensus step} without @reachin{PART} executing this @tech{consensus transfer}.
The @tech{continuation} of a @tech{consensus transfer} @tech{statement} is a @tech{consensus step}, which is finalized with a @tech{commit statement}.
The @tech{continuation} of a timeout block is the same as the continuation of the function the timeout occurs within.

@margin-note{See @seclink["guide-timeout"]{the guide section on non-participation} to understand when to use timeouts and how to use them most effectively.}

The @reachin{publish} component exclusive-or the @reachin{pay} component may be omitted, if either there is no @tech{publication} or no @tech{transfer} of @tech{network tokens} to accompany this @tech{consensus transfer}.
The @reachin{when} component may always be omitted, in which case it is assumed to be @reachin{true}.
@reachin{publish} or @reachin{pay} must occur first, after which components may occur in any order.
For example, the following are all @tech{valid}:

@reach{
 Alice.publish(coinFlip);

 Alice.pay(penaltyAmount);

 Alice.pay(penaltyAmount).publish(coinFlip);

 Alice.publish(coinFlip)
      .timeout(DELAY, () => closeTo(Bob, () => exit()));

 Alice.pay(penaltyAmount)
      .timeout(DELAY, () => {
        Bob.publish();
        commit();
        exit(); });

 Alice.publish(bid).when(wantsToBid);

}

The @reachin{timeout} component must be included if @reachin{when} is not statically @reachin{true}.
This ensures that your clients will eventually complete the program.
If a @tech{consensus transfer} is a guaranteed race between non-class @tech{participants} and a @tech{participant class} that @emph{may} attempt to transfer (i.e. @reachin{when} is not statically @reachin{false}), then a @reachin{timeout} may be explicitly omitted by writing @reachin{.timeout(false)}.

@reachin{.throwTimeout} may be used in place of @reachin{.timeout}. It accepts a @reachin{DELAY_EXPR} and an @reachin{EXPR}, which will be thrown if a timeout should occur.
If an @reachin{EXPR} is not provided, then @reachin{null} will be thrown.
If a @tech{consensus transfer} uses @reachin{.throwTimeout}, it must be within a @tech{try statement}.

If a @tech{consensus transfer} specifies a single @tech{participant}, which has not yet been @tech{fixed} in the application and is not a @tech{participant class}, then this statement does so; therefore, after it the @reachin{PART} may be used as an @tech{address}.

If a @tech{consensus transfer} specificies a single @tech{participant class}, then all members of that class will attempt to perform the transfer, but only one will succeed.

A @tech{consensus transfer} binds the identifiers @reachin{ID_0} through @reachin{ID_n} for all @tech{participants} to the values included in the @tech{consensus transfer}.
If an existing @tech{participant}, not included in @reachin{PART_EXPR}, has previously bound one of these identifiers, then the program is not @tech{valid}. In other words, the following program is not valid:

@reach{
 Alice.only(() => {
  const x = 1; });
 Bob.only(() => {
  const x = 2; });
 Claire.only(() => {
  const x = 3; });
 race(Alice, Bob).publish(x);
 commit();
}

because @reachin{Claire} is not included in the @reachin{race}.
However, if we were to rename @reachin{Claire}'s @reachin{x} into @reachin{y}, then it would be valid, because although @reachin{Alice} and @reachin{Bob} both bind @reachin{x}, they participate in the @reachin{race}, so it is allowed.
In the tail of this program, @reachin{x} is bound to either @reachin{1} or @reachin{2}.

@subsubsection{@tt{fork}}

@(mint-define! '("fork"))
@reach{
fork()
.case(Alice, (() => ({
  msg: 19,
  when: declassify(interact.keepGoing()) })),
  ((v) => v),
  (v) => {
    require(v == 19);
    transfer(wager + 19).to(this);
    commit();
    exit();
  })
.case(Bob, (() => ({
  when: declassify(interact.keepGoing()) })),
  ((_) => wager),
  (_) => {
    commit();

    Alice.only(() => interact.showOpponent(Bob));

    race(Alice, Bob).publish();
    transfer(2 * wager).to(this);
    commit();
    exit();
  })
.timeout(deadline, () => {
  race(Alice, Bob).publish();
  transfer(wager).to(this);
  commit();
  exit(); });
}

A @deftech{fork statement} is written:

@reach{
fork()
.paySpec(TOKENS_EXPR)
.case(PART_EXPR,
  PUBLISH_EXPR,
  PAY_EXPR,
  CONSENSUS_EXPR)
.timeout(DELAY_EXPR, () =>
  TIMEOUT_BLOCK);
}

where:
@reachin{TOKENS_EXPR} is an expression that evalues to a tuple of @reachin{Token}s.
@reachin{PART_EXPR} is an expression that evaluates to a @tech{participant};
@reachin{PUBLISH_EXPR} is a syntactic @tech{arrow expression} that is evaluated in a @tech{local step} for the specified @tech{participant} and must evaluate to an object that may contain a @litchar{msg} field, which may be of any type, and a @litchar{when} field, which must be a boolean;
@reachin{PAY_EXPR} is an expression that evaluates to a function parameterized over the @litchar{msg} value and returns a @tech{pay amount};
@reachin{CONSENSUS_EXPR} is a syntactic @tech{arrow expression} parameterized over the @litchar{msg} value which is evaluated in a @tech{consensus step}; and,
the @reachin{timeout} and @reachin{throwTimeout} parameter are as in an @tech{consensus transfer}.

If the @litchar{msg} field is absent from the object returned from @reachin{PUBLISH_EXPR}, then it is treated as if it were @reachin{null}.

If the @litchar{when} field is absent from the object returned from @reachin{PUBLISH_EXPR}, then it is treated as if it were @reachin{true}.

If the @reachin{PAY_EXPR} is absent, then it is treated as if it were @reachin{(_) => 0}.

The @reachin{.case} component may be repeated many times.

The same @tech{participant} may specify multiple cases. In this situation, the order of the cases is significant.
That is, a subsequent case will only be evaluated if the prior case's @tt{when} field is @reachin{false}.

If the @tech{participant} specified by @reachin{PART_EXPR} is not already @tech{fixed} (in the sense of @reachin{Participant.set}), then if it wins the @reachin{race}, it is @tech{fixed}, provided it is not a @tech{participant class}.

@(hrule)

A @tech{fork statement} is an abbreviation of a common @reachin{race} and @reachin{switch} pattern you could write yourself.

The idea is that each of the @tech{participants} in the @reachin{case} components do an independent @tech{local step} evaluation of a value they would like to @reachin{publish} and then all @reachin{race} to @reachin{publish} it.
The one that "wins" the @reachin{race} then determines not only the value (& @reachin{pay} amount), but also what @tech{consensus step} code runs to consume the value.

The sample @reachin{fork} statement linked to the @reachin{fork} keyword is roughly equivalent to:
@reach{
 // We first define a Data instance so that each participant can publish a
 // different kind of value
 const ForkData = Data({Alice: UInt, Bob: Null});
 // Then we bind these values for each participant
 Alice.only(() => {
  const fork_msg = ForkData.Alice(19);
  const fork_when = declassify(interact.keepGoing()); });
 Bob.only(() => {
  const fork_msg = ForkData.Bob(null);
  const fork_when = declassify(interact.keepGoing()); });
 // They race
 race(Alice, Bob)
  .publish(fork_msg)
  .when(fork_when)
  // The pay ammount depends on who is publishing
  .pay(fork_msg.match( {
    Alice: (v => v),
    Bob: ((_) => wager) } ))
  // The timeout is always the same
  .timeout(deadline, () => {
    race(Alice, Bob).publish();
    transfer(wager).to(this);
    commit();
    exit(); });

  // We ensure that the correct participant published the correct kind of value
  require(fork_msg.match( {
    // Alice had previously published
    Alice: (v => this == Alice),
    // But Bob had not.
    Bob: ((_) => true) } ));

  // Then we select the appropriate body to run
  switch (fork_msg) {
    case Alice: {
      assert (this == Alice);
      require(v == 19);
      transfer(wager + 19).to(this);
      commit();
      exit(); }
    case Bob: {
      Bob.set(this);
      commit();

      Alice.only(() => interact.showOpponent(Bob));

      race(Alice, Bob).publish();
      transfer(2 * wager).to(this);
      commit();
      exit(); }
  }
}

This pattern is tedious to write and error-prone, so the @reachin{fork} statement abbreviates it for Reach programmers.
When a @tech{participant} specifies multiple cases, the @tt{msg} field of the participant will be wrapped with an additional
variant signifying what case was chosen.

@subsubsection{@tt{wait}}

@(mint-define! '("wait"))
@reach{
 wait(AMOUNT); }

A @deftech{wait statement}, written @reachin{wait(AMOUNT);}, delays the computation until @reachin{AMOUNT} @tech{time delta} units have passed.
It may only occur in a @tech{step}.

@subsubsection{@tt{exit}}

@(mint-define! '("exit"))
@reach{
 exit(); }

An @deftech{exit statement}, written @reachin{exit();}, halts the computation.
It is a @tech{terminator statement}, so it must have an empty @tech{tail}.
It may only occur in a @tech{step}.

@subsection[#:tag "ref-programs-step-exprs"]{Expressions}

Any expressions valid for a @seclink["ref-programs-compute-exprs"]{computation} are valid for a step.
However, some additional expressions are allowed.

@subsubsection{@tt{race}}

@(mint-define! '("race"))
@reach{
 race(Alice, Bob).publish(bet); }

A @deftech{race expression}, written @reachin{race(PARTICIPANT_0, ..., PARTICIPANT_n);}, constructs a @tech{participant} that may be used in a @tech{consensus transfer} statement, such as @reachin{publish} or @reachin{pay}, where the various @tech{participants} race to be the first one to perform the @tech{consensus transfer}.

Reach provides a shorthand, @reachin{Anybody}, which serves as a @reachin{race} between all the @tech{participants}.

@margin-note{See @seclink["guide-race"]{the guide section on races} to understand the benefits and dangers of using @reachin{race}.}

@subsubsection{@tt{unknowable}}

@(mint-define! '("unknowable"))
@reach{
 unknowable( Notter, Knower(var_0, ..., var_N), [msg] ) }

@index{unknowable} A @tech{knowledge assertion} that the @tech{participant} @reachin{Notter} @emph{does not} know the results of the variables @reachin{var_0} through @reachin{var_N}, but that the @tech{participant} @reachin{Knower} @emph{does} know those values.
It accepts an optional bytes argument, which is included in any reported violation.

@subsubsection{@tt{closeTo}}

@(mint-define! '("closeTo"))
@reach{
 closeTo( Who, after, nonNetPayAmt ) }

@index{closeTo} Has @tech{participant} @reachin{Who} make a @tech{publication}, then @tech{transfer} the @reachin{balance()} and the non-network @tech{pay amount} to @reachin{Who} and end the @|DApp| after executing the function @reachin{after} in a @tech{step}.
The @reachin{nonNetPayAmt} parameter should be a @tech{pay amount}. For example, when closing a program that uses a @reachin{Token} @reachin{token}, the argument would be @reachin{[ [balance(tok), tok] ]}.
The @reachin{after} and @reachin{nonNetPayAmt} argument are optional.


@section[#:tag "ref-programs-local"]{Local Steps}

A Reach @tech{local step} occurs in the body of @reachin{only} or @reachin{each} statements.
It represents the actions taken by a single participant in an application.

@subsection[#:tag "ref-programs-local-stmts"]{Statements}

Any statements valid for a @seclink["ref-programs-compute-stmts"]{computation} are valid for a local step.

@subsection[#:tag "ref-programs-local-exprs"]{Expressions}

Any expressions valid for a @seclink["ref-programs-compute-exprs"]{computation} are valid for a local step.
However, some additional expressions are allowed.

@subsubsection[#:tag "ref-programs-local-this"]{@tt{this}}

Inside of a @tech{local step}, @reachin{this} refers to the participant performing the step.
This is useful when the @tech{local step} was initiated by an @reachin{each} expression.

@subsubsection{@tt{interact}}

@(mint-define! '("interact"))
@reach{
 interact.amount
 interact.notify(handA, handB)
 interact.chooseAmount(heap1, heap2) }

An @deftech{interaction expression}, written @reachin{interact.METHOD(EXPR_0, ..., EXPR_n)}, where @reachin{METHOD} is an identifier bound in the @tech{participant interact interface} to a function type, and @reachin{EXPR_0} through @reachin{EXPR_n} are @tech{expressions} that evaluates to the result of an @tech{interact}ion with a @tech{frontend} that receives the evaluation of the @reachin{n} @tech{expressions} and sends a @tech{value}.

An @tech{interaction expression} may also be written @reachin{interact.KEY}, where @reachin{KEY} is bound in the @tech{participant interact interface} to a non-function type.

An @tech{interaction expression} may only occur in a @tech{local step}.

@subsubsection{@tt{assume}}

@(mint-define! '("assume"))
@reach{
 assume( claim, [msg] ) }

@index{assume} An @tech{assumption} where @reachin{claim} evaluates to @reachin{true} with @tech{honest} @tech{frontends}.
This may only appear in a @tech{local step}.
It accepts an optional bytes argument, which is included in any reported violation.

@subsubsection{@tt{fail}}

@(mint-define! '("fail"))
@reach{
 fail() }

@index{fail} is a convenience method equivalent to @reachin{assume(false)}. This may only appear in a @tech{local step}.

@subsubsection{@tt{declassify}}

@(mint-define! '("declassify"))
@reach{
 declassify( arg ) }

The @deftech{declassify} primitive performs a @tech{declassification} of the given argument.

@subsubsection{@tt{makeCommitment}}

@(mint-define! '("makeCommitment"))
@reach{
 makeCommitment( interact, x ) }

@index{makeCommitment} Returns two values, @reachin{[ commitment, salt ]}, where @reachin{salt} is the result of calling @reachin{interact.random()}, and
@reachin{commitment} is the @tech{digest} of @reachin{salt} and @reachin{x}.
This is used in a @tech{local step} before @reachin{checkCommitment} is used in a @tech{consensus step}.

@section[#:tag "ref-programs-consensus"]{Consensus Steps}

A Reach @tech{consensus step} occurs in the @tech{continuation} of a @tech{consensus transfer} statement.
It represents the actions taken by the @tech{consensus network} @tech{contract} of an application.

@subsection[#:tag "ref-programs-consensus-stmts"]{Statements}

Any statements valid for a @seclink["ref-programs-compute-stmts"]{computation} are valid for a consensus step.
However, some additional statements are allowed.

@subsubsection{@tt{commit}}

@(mint-define! '("commit"))
@reach{
 commit(); }

A @deftech{commit statement}, written @reachin{commit();}, @tech{commits} to @tech{statement}'s @tech{continuation} as the next @tech{step} of the @DApp computation. In other words, it ends the current @tech{consensus step} and allows more @tech{local steps}.

@subsubsection[#:tag "ref-programs-only-consensus"]{@tt{only} and @tt{each}}

@secref["ref-programs-only-step"] are allowed in @tech{consensus steps} and are executed by @tech{backends} once they observe the completion of the @tech{consensus step} (i.e., after the associated @tech{commit statement}.)

@subsubsection[#:tag "ref-programs-consensus-view"]{View Objects}

@reach{
  vNFT.owner.set(creator);
}

If @reachin{VIEW} is a @deftech{view object}, then its fields are the elements of the associated @tech{view}.
Each of these fields are bound to an object with an @litchar{set} method that accepts the function or value to be bound to that @tech{view} at the current step, and all steps dominated by the current step (unless otherwise overridden.)
If this function is not provided with an argument, then the corresponding @tech{view} is unset.

For example, consider the following program:

@reachex[#:show-lines? #t "view-steps/index.rsh"
         #:link #t]

In this program, the Reach backend calls the frontend @reachin{interact} function, @reachin{checkView} with the expected value of the @tech{views} at each point in the program.
The frontend compares that value with what is returned by
@js{
[ await ctc.getViews().Main.last(),
  await ctc.getViews().Main.i() ]
}

When a @tech{view} is bound to a function, it may inspect any values in its scope, including @tech{linear state}.

@subsubsection{@tt{Participant.set} and @tt{.set}}

@(mint-define! '("Participant.set"))
@reach{
 Participant.set(PART, ADDR);
 PART.set(ADDR); }

@index{Participant.set} After execution, the given @tech{participant} is @tech{fixed} to the given address.
It is @tech{invalid} to attempt to @reachin{.set} a @tech{participant class}.
If a @tech{backend} is running for this @tech{participant} and its address does not match the given address, then it will abort.
This may only occur within a @tech{consensus step}.

@margin-note{@secref["workshop-relay"] is a good introductory project that demonstrates how to use this feature of Reach.}

@subsubsection{@tt{while}}

@(mint-define! '("while") '("var") '("invariant"))
@reach{
 var [ heap1, heap2 ] = [ 21, 21 ];
 { const sum = () => heap1 + heap2; }
 invariant(balance() == 2 * wagerAmount);
 while ( sum() > 0 ) {
   ....
   [ heap1, heap2 ] = [ heap1 - 1, heap2 ];
   continue; } }

A @deftech{while statement} may occur within a @tech{consensus step} and is written:

@reach{
 var LHS = INIT_EXPR;
 BLOCK; // optional
 invariant(INVARIANT_EXPR);
 while( COND_EXPR ) BLOCK }

where @reachin{LHS} is a valid left-hand side of an @tech{identifier definition} where the @tech{expression} @reachin{INIT_EXPR} is the right-hand side, and
@reachin{BLOCK} is an optional @tech{block} that may define bindings that use the @reachin{LHS} values which are bound inside the rest of the @reachin{while} and its @tech{tail}, and
@reachin{INVARIANT_EXPR} is an @tech{expression}, called the @deftech{loop invariant}, that must be true before and after every execution of the @tech{block} @reachin{BLOCK}, and
if @reachin{COND_EXPR} is true, then the @tech{block} executes,
and if not, then the loop terminates and control transfers to the @tech{continuation} of the @tech{while statement}.
The identifiers bound by @reachin{LHS} are bound within @reachin{INVARIANT_EXPR}, @reachin{COND_EXPR}, @reachin{BLOCK}, and the @tech{tail} of the @tech{while statement}.

@margin-note{Read about finding @seclink["guide-loop-invs"]{loop invariants} in the Reach guide.}

@subsubsection{@tt{continue}}

@(mint-define! '("continue"))
@reach{
 [ heap1, heap2 ] = [ heap1 - 1, heap2 ];
 continue; }

A @deftech{continue statement} may occur within a @tech{while statement}'s @tech{block} and is written:

@reach{
 LHS = UPDATE_EXPR;
 continue; }

where the identifiers bound by @reachin{LHS} are a subset of the variables bound by the nearest enclosing @tech{while statement} and @reachin{UPDATE_EXPR} is an @tech{expression} which may be bound by @reachin{LHS}.

A @tech{continue statement} is a @tech{terminator statement}, so it must have an empty @tech{tail}.

A @tech{continue statement} may be written without the preceding identifier update, which is equivalent to writing

@reach{
 [] = [];
 continue; }

A @tech{continue statement} must be dominated by a @tech{consensus transfer}, which means that the body of a @tech{while statement} must always @reachin{commit();} before calling @reachin{continue;}.
This restriction may be lifted in future versions of Reach, which will perform termination checking.

@subsubsection{@tt{parallelReduce}}

@(mint-define! '("parallelReduce"))
@reach{
const [ keepGoing, as, bs ] =
  parallelReduce([ true, 0, 0 ])
  .invariant(balance() == 2 * wager)
  .while(keepGoing)
  .case(Alice, (() => ({
    when: declassify(interact.keepGoing()) })),
    (_) => {
      each([Alice, Bob], () => {
        interact.roundWinnerWas(true); });
      return [ true, 1 + as, bs ]; })
  .case(Bob, (() => ({
    when: declassify(interact.keepGoing()) })),
    (_) => {
      each([Alice, Bob], () => {
        interact.roundWinnerWas(false); });
      return [ true, as, 1 + bs ]; })
  .timeout(deadline, () => {
    showOutcome(TIMEOUT)();
    race(Alice, Bob).publish();
    return [ false, as, bs ]; });
}

A @deftech{parallel reduce statement} is written:

@reach{
const LHS =
  parallelReduce(INIT_EXPR)
  .invariant(INVARIANT_EXPR)
  .while(COND_EXPR)
  .paySpec(TOKENS_EXPR)
  .case(PART_EXPR,
    PUBLISH_EXPR,
    PAY_EXPR,
    CONSENSUS_EXPR)
  .timeout(DELAY_EXPR, () =>
    TIMEOUT_BLOCK);
}

The @reachin{LHS} and @reachin{INIT_EXPR} are like the initialization component of a @reachin{while} loop; and,
the @reachin{.invariant} and @reachin{.while} components are like the invariant and condition of a @reachin{while} loop;
while the @reachin{.case}, @reachin{.timeout}, and @reachin{.paySpec} components are like the corresponding components of a @reachin{fork} statement.

The @reachin{.case} component may be repeated many times.

The same participant may specify multiple cases; the order of the cases is significant, just like in a @reachin{fork} statement.

@subsubsub*section{@tt{.timeRemaining}}

When dealing with absolute deadlines in @reachin{parallelReduce}, there is a common pattern in the
@reachin{TIMEOUT_BLOCK} to have participants @reachin{race} to @reachin{publish} and return the accumulator.
There is a shorthand, @reachin{.timeRemaining}, available for this situation:

@(mint-define! '("timeRemaining"))
@reach{
  const [ timeRemaining, keepGoing ] = makeDeadline(deadline);
  const [ x, y, z ] =
    parallelReduce([ 1, 2, 3 ])
      .while(keepGoing())
      ...
      .timeRemaining(timeRemaining()) }

which will expand to:

@reach{
  .timeout(timeRemaining(), () => {
    race(...Participants).publish();
    return [ x, y, z ]; }) }

@subsubsub*section{@tt{.throwTimeout}}

@reachin{.throwTimeout} is a shorthand that will throw the accumulator as an exception when a @tech{timeout} occurs.
Therefore, a @reachin{parallelReduce} that uses this branch must be inside of a @tech{try statement}. For example,

@(mint-define! '("throwTimeout"))
@reach{
  try {
    const [ x, y, z ] =
      parallelReduce([ 1, 2, 3 ])
      ...
      .throwTimeout(deadline)
  } catch (e) { ... } }

 will expand @reachin{throwTimeout} to:

@reach{
  .timeout(deadline, () => {
    throw [ x, y, z ]; }) }


@(hrule)

A @tech{parallel reduce statement} is essentially an abbreviation of pattern of a @reachin{while} loop combined with a @reachin{fork} statement that you could write yourself.
This is an extremely common pattern in decentralized applications.

The idea is that there are some values (the @reachin{LHS}) which after intialization will be repeatedly updated uniquely by each of the racing @tech{participants} until the condition does not hold.

@reach{
var LHS = INIT_EXPR;
invariant(INVARIANT_EXPR)
while(COND_EXPR) {
  fork()
  .case(PART_EXPR,
    PUBLISH_EXPR,
    PAY_EXPR,
    (m) => {
      LHS = CONSENSUS_EXPR(m);
      continue; })
  .timeout(DELAY_EXPR, () =>
    TIMEOUT_BLOCK);
}
}

@subsection[#:tag "ref-programs-consensus-exprs"]{Expressions}

Any expressions valid for a @seclink["ref-programs-compute-exprs"]{computation} are valid for a consensus step.
However, some additional expressions are allowed.

@subsubsection[#:tag "ref-programs-consensus-this"]{@tt{this}}

Inside of a @tech{consensus step}, @reachin{this} refers to the address of the participant that performed the @tech{consensus transfer}.
This is useful when the @tech{consensus transfer} was initiated by a @reachin{race} expression.

@subsubsection{@tt{transfer}}

@(mint-define! '("transfer"))
@reach{
 transfer(10).to(Alice);
 transfer(2, gil).to(Alice); }

A @deftech{transfer expression},
written @reachin{transfer(AMOUNT_EXPR).to(PART)},
where @reachin{AMOUNT_EXPR} is an @tech{expression} that evaluates to an unsigned integer, and
@reachin{PART} is a @tech{participant} identifier,
performs a @tech{transfer} of @tech{network tokens} from the @tech{contract} to the named @tech{participant}.
@reachin{AMOUNT_EXPR} must evaluate to less than or equal to the balance of @tech{network tokens} in the @tech{contract} @tech{account}.

A @tech{transfer expression} may also be written @reachin{transfer(AMOUNT_EXPR, TOKEN_EXPR).to(PART)},
where @reachin{TOKEN_EXPR} is a @reachin{Token},
which @tech{transfers} @tech{non-network tokens} of the specified type.

A @tech{transfer expression} may only occur within a @tech{consensus step}.

@subsubsection{@tt{require}}

@(mint-define! '("require"))
@reach{
 require( claim, [msg] ) }

@index{require} A @tech{requirement} where @reachin{claim} evaluates to @reachin{true} with @tech{honest} @tech{participants}.
This may only appear in a @tech{consensus step}.
It accepts an optional bytes argument, which is included in any reported violation.

@subsubsection{@tt{checkCommitment}}

@(mint-define! '("checkCommitment"))
@reach{
 checkCommitment( commitment, salt, x ) }

@index{checkCommitment} Makes a @tech{requirement} that @reachin{commitment} is the @tech{digest} of @reachin{salt} and @reachin{x}.
This is used in a @tech{consensus step} after @reachin{makeCommitment} was used in a @tech{local step}.

@subsubsection{Remote objects}

@(mint-define! '("remote"))
@reach{
  const randomOracle =
    remote( randomOracleAddr, {
      getRandom: Fun([], UInt),
    });
  const randomVal = randomOracle.getRandom.pay(randomFee)();
}

A @deftech{remote object} is representation of a foreign @tech{contract} in a Reach application.
During a @tech{consensus step}, a Reach computation may consensually communicate with such an object via a prescribed interface.

A @tech{remote object} is constructed by calling the @reachin{remote} function with an @tech{address} and an interface---an object where each key is bound to a @tech{function type}. For example:
@reach{
  const randomOracle =
    remote( randomOracleAddr, {
      getRandom: Fun([], UInt),
    });
  const token =
    remote( tokenAddr, {
      balanceOf: Fun([Address], UInt),
      transferTo: Fun([UInt, Addres], Null),
    });
}

Once constructed, the fields of a @tech{remote object} represent those remote contract interactions, referred to as @deftech{remote functions}.
For example, @reachin{randomOracle.getRandom}, @reachin{token.balanceOf}, and @reachin{token.transferTo} are @tech{remote functions} in the example.

A @tech{remote function} may be invoked by calling it with the appropriate arguments, whereupon it returns the specified output.
In addition, a @tech{remote function} may be augmented with one of the following operations:

@itemize[

@item{@reachin{REMOTE_FUN.pay(AMT)} --- Returns a @tech{remote function} that receives a @tech{pay amount}, @reachin{AMT}, @emph{from} the caller when it is called.}

@item{@(mint-define! '("bill")) @reachin{REMOTE_FUN.bill(AMT)} --- Returns a @tech{remote function} that provides a @tech{pay amount}, @reachin{AMT}, @emph{to} the caller when it returns.}

@item{@(mint-define! '("withBill")) @reachin{REMOTE_FUN.withBill()} --- Returns a @tech{remote function} that provides some number of @tech{network tokens} and, possibly, @tech{non-network tokens} @emph{to} the caller when it returns.
The exact amount is returned from the invocation by wrapping the original result in a tuple.

If the remote contract is not expected to return @tech{non-network tokens} then a pair is returned, where the amount of @tech{network tokens} received is the first element, and the original result is the second element.

If the remote contract is expected to return @tech{non-network tokens} then a triple is returned, where the amount of @tech{network tokens} received
is the first element, a tuple of the @tech{non-network tokens} received is the second element, and the original result is the third element.
If the caller expects to receive @tech{non-network tokens}, they must provide a tuple of tokens as an argument to @reachin{withBill}. The ordering of
tokens in the argument is reserved when returning the amounts received.
For example,

@reach{
 const [ returned, [gilRecv, zmdRecv], randomValue ] =
   randomOracle.getRandom.pay(stipend).withBill([gil, zmd])();
}

might be the way to communicate with a random oracle that receives a conservative approximation of its actual cost and returns what it does not use, along with some amount of @tt{GIL} and @tt{ZMD}.
This operation may not be used with @reachin{REMOTE_FUN.bill}.}

]

@subsubsection{Mappings: creation and modification}

@(mint-define! '("Map"))
@reach{
  const bidsM = new Map(UInt);
  bidsM[this] = 17;
  delete bidsM[this];
}

A new @tech{mapping} of @tech{linear state} may be constructed in a @tech{consensus step} by writing @reachin{new Map(TYPE_EXPR)}, where @reachin{TYPE_EXPR} is some @tech{type}.

This returns a value which may be used to dereference particular mappings via @reachin{map[ADDR_EXPR]}, where @reachin{ADDR_EXPR} is an @tech{address}.
Such dereferences return a value of type @reachin{Maybe(TYPE_EXPR)}, because the @tech{mapping} may not contain a value for @reachin{ADDR_EXPR}.

A @tech{mapping} may be modified by writing @reachin{map[ADDR_EXPR] = VALUE_EXPR} to install @reachin{VALUE_EXPR} (of type @reachin{TYPE_EXPR}) at @reachin{ADDR_EXPR}, or by writing @reachin{delete map[ADDR_EXPR]} to remove the mapping entry.
Such modifications may only occur in a @tech{consensus step}.

@subsubsection{Sets: creation and modification}

@(mint-define! '("Set") '("insert") '("remove") '("member"))
@reach{
  const bidders = new Set();
  bidders.insert(Alice);
  bidders.remove(Alice);
  bidders.member(Alice); // false
}

A @reachin{Set} is another container for @tech{linear state}. It is simply a type alias of @reachin{Map(Null)};
it is only useful for tracking @reachin{Address}es. Because a @reachin{Set} is internally a @reachin{Map}, it may
only be constructed in a @tech{consensus step}.

A @reachin{Set} may be modified by writing @reachin{s.insert(ADDRESS)} to install @reachin{ADDRESS} in the
set, @reachin{s}, or @reachin{s.remove(ADDRESS)} to remove the @reachin{ADDRESS} from the set.
Such modifications may only occur in a @tech{consensus step}.

@reachin{s.member(ADDRESS)} will return a @reachin{Bool} representing whether the address is in the set.

@section[#:tag "ref-programs-compute"]{Computations}

This section describes the common features available in all Reach contexts.

@subsection{Comments}

@reach{
 // single-line comment
 /* multi-line
  * comment
  */ }

Comments are text that is ignored by the compiler.
Text starting with @litchar{//} up until the end of the line forms a @deftech{single-line comment}.
Text enclosed with @litchar{/*} and @litchar{*/} forms a @deftech{multi-line comment}.
It is @tech{invalid} to nest a @tech{multi-line comment} within a @tech{multi-line comment}.

@subsection{Blocks}

@reach{
  { return 42; }
  { const x = 31;
    return x + 11; }
  { if ( x < y ) {
      return "Why";
    } else {
      return "Ecks"; } } }

A @deftech{block} is a sequence of @tech{statements} surrounded by braces, i.e. @litchar["{"] and @litchar["}"].

@subsection[#:tag "ref-programs-compute-stmts"]{Statements}

This section describes the @deftech{statements} which are allowed in any Reach context.

Each @tech{statement} affects the meaning of the subsequent @tech{statements}, which is called its @deftech{tail}. For example, if @reachin{{X; Y; Z;}} is a @tech{block}, then @reachin{X}'s @tech{tail} is @reachin{{Y; Z;}} and @reachin{Y}'s @tech{tail} is @reachin{{Z;}}.

Distinct from @tech{tails} are @deftech{continuations} which include everything after the @tech{statement}. For example, in @reachin{{{X; Y;}; Z;}}, @reachin{X}'s @tech{tail} is just @reachin{Y}, but its @tech{continuation} is @reachin{{Y;}; Z;}.

@tech{Tails} are statically apparent from the structure of the program source code, while @tech{continuations} are influenced by function calls.

A sequence of @tech{statements} that does not end in a @deftech{terminator statement} (a @tech{statement} with no @tech{tail}), such as a @tech{return statement}, @tech{continue statement}, or @tech{exit statement} is treated as if it ended with @reachin{return null;}.

The remainder of this section enumerates each kind of @tech{statement}.

@subsubsection{@tt{const} and @tt{function}}

An @deftech{identifier definition} is either
a @tech{value definition}
or a @tech{function definition}.
Each of these introduces one or more @deftech{bound identifier}s.

@(hrule)

@(mint-define! '("const"))
@reach{
  const DELAY = 10;
  const [ Good, Bad ] = [ 42, 43 ];
  const { x, y } = { x: 1, y: 2 };
  const [ x, [ y ] ] = [ 1, [ 2 ] ];
  const [ x, { y } ] = [ 1, { y: 2 } ];
  const { x: [ a, b ] } = { x: [ 1, 2 ] };
  }
@margin-note{@tech{Valid} @deftech{identifiers} follow the same rules as JavaScript identifiers:
they may consist of Unicode alphanumeric characters,
or @reachin{_} or @reachin{$},
but may not begin with a digit.}

A @deftech{value definition} is written @reachin{const LHS = RHS;}.

@reachin{LHS} must obey the grammar:

@BNF[
(list
 @nonterm{LHS}
 @nonterm{id}
 @BNF-seq[@litchar["["] @nonterm{LHS-tuple-seq} @litchar["]"]]
 @BNF-seq[@litchar["{"] @nonterm{LHS-obj-seq} @litchar["}"]])
(list
 @nonterm{LHS-tuple-seq}
 @BNF-seq[]
 @BNF-seq[@litchar["..."] @nonterm{LHS}]
 @BNF-seq[@nonterm{LHS}]
 @BNF-seq[@nonterm{LHS} @litchar[","] @nonterm{LHS-tuple-seq}])
(list
 @nonterm{LHS-obj-seq}
 @BNF-seq[]
 @BNF-seq[@litchar["..."] @nonterm{LHS}]
 @BNF-seq[@nonterm{LHS-obj-elem}]
 @BNF-seq[@nonterm{LHS-obj-elem} @litchar[","] @nonterm{LHS-obj-seq}])
(list
 @nonterm{LHS-obj-elem}
 @BNF-seq[@nonterm{id}]
 @BNF-seq[@nonterm{propertyName} @litchar[":"] @nonterm{LHS}])
(list
 @nonterm{propertyName}
 @nonterm{id}
 @nonterm{string}
 @nonterm{number}
 @BNF-seq[@litchar["["] @nonterm{expr} @litchar["]"]])
]

@reachin{RHS} must be compatible with the given @reachin{LHS}.
That is, if a @reachin{LHS} is an @nonterm{LHS-tuple-seq}, then the corresponding @reachin{RHS} must be a tuple with the correct number of elements.
If a @reachin{LHS} is an @nonterm{LHS-obj-seq}, then the corresponding @reachin{RHS} must be an object with the correct fields.

Those @tech{values} are available as their corresponding @tech{bound identifier}s in the statement's @tech{tail}.

@(hrule)

@(mint-define! '("function"))
@reach{
  function randomBool() {
    return (interact.random() % 2) == 0; }; }

A @deftech{function definition}, written @reachin{function FUN(LHS_0, ..., LHS_n) BLOCK;}, defines @reachin{FUN} as a function which abstracts its @deftech{function body}, the @tech{block} @reachin{BLOCK}, over the left-hand sides @reachin{LHS_0} through @reachin{LHS_n}.

Function parameters may specify default arguments. The expressions used to instantiate these parameters
have access to any variables in the scope of which the function was defined. Additionally, these expressions
may reference previous arguments of the @tech{function definition}.
Parameters with default arguments must come after all other parameters.

@reach{
  function f(a, b, c = a + 1, d = b + c) =>
    a + b + c + d;
}

The last parameter of a function may be a @deftech{rest parameter}, which allows the function to be called
with an arbitrary number of arguments. A @tech{rest parameter} is specified via @reachin{...IDENT}, where
@reachin{IDENT} is bound to a @reachin{Tuple} containing all the remaining arguments.

@(hrule)

All identifiers in Reach programs must be @deftech{unbound}
at the position of the program where they are bound,
i.e., it is @tech{invalid} to shadow identifiers with new definitions.
For example,

@reach{
 const x = 3;
 const x = 4; }

is @tech{invalid}.
This restriction is independent of whether a binding is
only known to a single @tech{participant}. For example,

@reach{
 Alice.only(() => {
   const x = 3; });
 Bob.only(() => {
   const x = 3; }); }

is @tech{invalid}.

The special identifier @reachin{_} is an exception to this rule.
The @reachin{_} binding is always considered to be unbound.
This means means that @reachin{_} is both
an identifier that can never be read,
as well as an identifier that may be bound many times.
This may be useful for ignoring unwanted values, for example:

@reach{
 const [_, x, _] = [1, 2, 3];
}

@subsubsection{@tt{return}}

@(mint-define! '("return"))
@reach{
 return 17;
 return 3 + 4;
 return f(2, false);
 return; }

A @deftech{return statement}, written @reachin{return EXPR;}, where @reachin{EXPR} is an @tech{expression} evaluates to the same @tech{value} as @reachin{EXPR}.
As a special case, @reachin{return;} is interpreted the same as @reachin{return null;}.

A @tech{return statement} returns its value to the surrounding function application.

A @tech{return statement} is a @tech{terminator statement}, so it must have an empty @tech{tail}. For example,

@reach{
 { return 1;
   return 2; } }

is @tech{invalid}, because the first @reachin{return}'s @tech{tail} is not empty.

@subsubsection{@tt{if}}

@(mint-define! '("if") '("else"))
@reach{
 if ( 1 + 2 < 3 ) {
   return "Yes!";
 } else {
   return "No, waaah!"; } }

A @deftech{conditional statement},
written @reachin{if (COND) NOT_FALSE else FALSE},
where @reachin{COND} is an @tech{expression}
and @reachin{NOT_FALSE} and @reachin{FALSE} as @tech{statements}
(potentially @tech{block statements}),
selects between the @reachin{NOT_FALSE} @tech{statement} and @reachin{FALSE} @tech{statement} based on whether @reachin{COND} evaluates to @reachin{false}.

Both @reachin{NOT_FALSE} and @reachin{FALSE} have empty @tech{tails}, i.e. the @tech{tail} of the @tech{conditional statement} is not propagated. For example,

@reach{
 if ( x < y ) {
   const z = 3; }
 else {
   const z = 4; }
 return z; }

is erroneous, because the identifier @reachin{z} is not bound outside the @tech{conditional statement}.

A @tech{conditional statement} may only include a @tech{consensus transfer} in @reachin{NOT_FALSE} or @reachin{FALSE} if it is within a @tech{consensus step}, because its statements are in the same context as the conditional statement itself.

@subsubsection{@tt{switch}}

@(mint-define! '("switch") '("case") '("default"))
@reach{
 const mi = Maybe(UInt).Some(42);
 switch ( mi ) {
  case None: return 8;
  case Some: return mi + 10; }
 switch ( mi ) {
  case None: return 8;
  default: return 41; } }

A @deftech{switch statement},
written @reachin{switch (VAR) { CASE ... }},
where @reachin{VAR} is a variable bound to a @tech{data instance}
and @reachin{CASE} is either @reachin{case VARIANT: STMT ...}, where @reachin{VARIANT} is a variant, or @reachin{default: STMT ...}, @reachin{STMT} is a sequence of statements,
selects the appropriate sequence of statements based on which variant @reachin{VAR} holds.
Within the body of a @reachin{switch} case, @reachin{VAR} has the type of variant; i.e. in a @reachin{Some} case of a @reachin{Maybe(UInt)} @reachin{switch}, the variable is bound to an integer.

All cases have empty @tech{tails}, i.e. the @tech{tail} of the @tech{switch statement} is not propagated.

A @tech{switch statement} may only include a @tech{consensus transfer} in its cases if it is within a @tech{consensus step}, because its statements are in the same context as the conditional statement itself.

It is @tech{invalid} for a case to appear multiple times, or be missing, or to be superfluous (i.e. for a variant that does not exist in the @reachin{Data} type of @reachin{VAR}).

@subsubsection{Block statements}

A @deftech{block statement} is when a @tech{block} occurs in a @tech{statement} position, then it establishes a local, separate scope for the definitions of identifiers within that @tech{block}. In other words, the @tech{block} is evaluated for effect, but the @tech{tail} of the @tech{statements} within the @tech{block} are isolated from the surrounding @tech{tail}. For example,

@reach{
 const x = 4;
 return x; }

evaluates to @reachin{4}, but

@reach{
 { const x = 4; }
 return x; }

is erroneous, because the identifier @reachin{x} is not bound outside the @tech{block statement}.

@subsubsection{Try/Catch & Throw Statements}

@reach{
  try {
    throw 10;
  } catch (v) {
    transfer(v).to(A); }}

A @deftech{try statement}, written @reachin{try BLOCK catch (VAR) BLOCK}, allows a block
of code to execute with a specified handler should an exception be thrown.

A @deftech{throw statement},
written @reachin{throw EXPR}, will transfer control flow to the exception handler, binding @tt{EXPR}
to @tt{VAR}. Any value that is able to exist at runtime may be thrown. For example, @reachin{Int}s
and @reachin{Array}s are valid values to throw, but a function is not.

@subsubsection{Expression statements}

@reach{
 4;
 f(2, true); }

An @tech{expression}, @reachin{E}, in a @tech{statement} position is equivalent to the @tech{block statement} @reachin{{ return E; }}.

@subsection[#:tag "ref-programs-compute-exprs"]{Expressions}

This section describes the expressions which are allowed in any Reach context.
There are a large variety of different @deftech{expressions} in Reach programs.

The remainder of this section enumerates each kind of @tech{expression}.

@subsubsection{'use strict'}

@(mint-define! '("'use strict'"))
@reach{
  'use strict'; }

@index{'use strict'} @reachin{'use strict'} enables unused variables checks for all subsequent
declarations within the current scope. If a variable is declared, but never used, there will
be an error emitted at compile time.

@deftech{strict mode} will reject some code that is normally valid and limit how dynamic Reach's type system is.
For example, normally Reach will permit expressions like the following to be evaluated:

@reach{
  const foo = (o) =>
    o ? o.b : false;

  void foo({ b: true });
  void foo(false); }

Reach allows @reachin{o} to be either an object with a @reachin{b} field or @reachin{false} because it
partially evaluates the program at compile time. So, without @reachin{'use strict'}, Reach will not evaluate
@reachin{o.b} when @reachin{o = false} and this code will compile successfully.

But, in @tech{strict mode}, Reach will ensure that this program treats @reachin{o} as
having a single type and detect an error in the program as follows:

@verbatim{
  reachc: error: Invalid field access. Expected object, got: Bool }

The correct way to write a program like this in @tech{strict mode} is to use @reachin{Maybe}. Like this:

@reach{
  const MObj = Maybe(Object({ b : Bool }));

  const foo = (mo) =>
    mo.match({
      None: (() => false),
      Some: ((o) => o.b)
    });

  void foo(MObj.Some({ b : true }));
  void foo(MObj.None()); }

@subsubsection{Identifier reference}

@reach{
 X
 Y
 Z }

An identifier, written @reachin{ID}, is an @tech{expression} that evaluates to the value of the @tech{bound identifier}.

@(mint-define! '("this"))
The identifier @reachin{this} has a special meaning inside of a @tech{local step} (i.e. the body of an @reachin{only} or @reachin{each} expression), as well as in a @tech{consensus step} (i.e. the tail of @reachin{publish} or @reachin{pay} statement and before a @reachin{commit} statement). For details, see @secref["ref-programs-local-this"] and @secref["ref-programs-consensus-this"].

@subsubsection{Function application}

@reach{
 assert( amount <= heap1 )
 step( moveA )
 digest( coinFlip )
 interact.random()
 declassify( _coinFlip ) }

A @deftech{function application}, written @reachin{EXPR_rator(EXPR_rand_0, ..., EXPR_rand_n)}, where @reachin{EXPR_rator} and @reachin{EXPR_rand_0} through @reachin{EXPR_rand_n} are @tech{expressions} that evaluate to one value.
@reachin{EXPR_rator} must evaluate to an abstraction over @reachin{n} values or a primitive of arity @reachin{n}.
A spread expression (@reachin{...expr}) may appear in the list of operands to a function application, in which case the elements of the expr are spliced in place.

@(mint-define! '("new"))
@reachin{new f(a)} is equivalent to @reachin{f(a).new()} and is a convenient short-hand for writing class-oriented programs.

@subsubsection{Types}

Reach's @deftech{type}s are represented with programs by the following identifiers and constructors:

@itemlist[
  @item{@(mint-define! '("Null")) @reachin{Null}.}
  @item{@(mint-define! '("Bool")) @reachin{Bool}, which denotes a boolean.}
  @item{@(mint-define! '("UInt")) @reachin{UInt}, which denotes an unsigned integer.
  @reachin{UInt.max} is the largest value that may be assigned to a @reachin{UInt}.}
  @item{@(mint-define! '("Bytes")) @reachin{Bytes(length)}, which denotes a string of bytes of length at most @reachin{length}.}
  @item{@(mint-define! '("Digest")) @reachin{Digest}, which denotes a @tech{digest}.}
  @item{@(mint-define! '("Address")) @reachin{Address}, which denotes an @tech{account} @tech{address}.}
  @item{@(mint-define! '("Token")) @reachin{Token}, which denotes a @tech{non-network token}.}
  @item{@(mint-define! '("Fun")) @reachin{Fun([Domain_0, ..., Domain_N], Range)}, which denotes a @deftech{function type}.
  The domain of a function is @tech{negative position}.
  The range of a function is @tech{positive position}.}
  @item{@(mint-define! '("Tuple")) @reachin{Tuple(Field_0, ..., FieldN)}, which denotes a tuple.
  (Refer to @secref["ref-programs-tuples"] for constructing tuples.)}
  @item{@(mint-define! '("Object")) @reachin{Object({key_0: Type_0, ..., key_N: Type_N})}, which denotes an @tech{object}.
  (Refer to @secref["ref-programs-objects"] for constructing objects.)}
  @item{@(mint-define! '("Struct")) @reachin{Struct([[key_0, Type_0], ..., [key_N, Type_N]])}, which denotes a @tech{struct}.
  (Refer to @secref["ref-programs-structs"] for constructing structs.)}
  @item{@(mint-define! '("Array")) @reachin{Array(Type_0, size)}, which denotes a statically-sized array.
  @reachin{Type_0} must be a type that can exist at runtime (i.e., not a @tech{function type}.)
  (Refer to @secref["ref-programs-arrays"] for constructing arrays.)}
  @item{@(mint-define! '("Data")) @reachin{Data({variant_0: Type_0, ..., variant_N: Type_N})}, which denotes a @link["https://en.wikipedia.org/wiki/Tagged_union"]{tagged union} (or @emph{sum type}).
  (Refer to @secref["ref-programs-data"] for constructing @tech{data instances}.)}
  @item{@(mint-define! '("Refine")) @reachin{Refine(Type_0, Predicate, ?Message)}, where @reachin{Predicate} is a unary function returning a boolean, which denotes a @link["https://en.wikipedia.org/wiki/Refinement_type"]{refinement type}, that is instances of @reachin{Type_0} that satisfy @reachin{Predicate}.
  When a refinement type appears in a @deftech{negative position} (such as in a @reachin{is} or in the domain of a @reachin{Fun} of a @tech{participant interact interface}), it introduces an @reachin{assert};
  while when it is in a @deftech{positive position}, it introduces an @reachin{assume}.
  @reachin{Message} is an optional string to display if the predicate fails verification.

  For example, if @reachin{f} had type @reach{Fun([Refine(UInt, (x => x < 5))], Refine(UInt, (x => x > 10)))}

  then @reachin{const z = f(y)} is equivalent to

  @reach{
    assert(y < 5);
    const z = f(y);
    assume(z > 10);}}
 @item{@reachin{Refine(Type_0, PreCondition, PostCondition, ?Messages)}, where @reachin{Type_0} is a @tech{function type}, @reachin{PreCondition} is a unary function that accepts a tuple of the domain and returns a boolean, and @reachin{PostCondition} is a binary function that accepts a tuple of the domain and the range and returns a boolean, denotes a @tech{function type} with a @link["https://en.wikipedia.org/wiki/Precondition"]{precondition} and @link["https://en.wikipedia.org/wiki/Postcondition"]{postcondition}.
 Preconditions are enforced with @reachin{assert} and postconditions are enforced with @reachin{assume}.
 @reachin{Messages} is an optional two-tuple of @reachin{Bytes}.
 The first message will be displayed when the precondition fails verification and the second when the postcondition fails verification.

 For example, @reachin{Refine(Fun([UInt, UInt], UInt), ([x, y] => x < y), (([x, y], z) => x + y < z))} is a function that requires its second argument to be larger than its first and its result to be larger than its input.}
]

@reachin{Object} and @reachin{Data} are commonly used to implemented @link["https://en.wikipedia.org/wiki/Algebraic_data_type"]{algebraic data types} in Reach.

@(mint-define! '("typeOf") '("isType") '("is"))
@reach{
 typeOf(x) // type
 isType(t) // Bool
 is(x, t) // t
}

The @reachin{typeOf} primitive function is the same as @reachin{typeof}:
it returns the type of its argument.

The @reachin{isType} function returns @reachin{true} if its argument is a type.
Any expression satisfying @reachin{isType} is compiled away and does not exist at runtime.

The @reachin{is} function returns its first argument if it satisfies the type specified by the second argument.
If it is not, then the program is @tech{invalid}.
For example, @reachin{is(5, UInt)} returns @reachin{5}, while @reachin{is(5, Bool)} is an @tech{invalid} program.
The value returned by @reachin{is} may not be identical to the input, because in some cases, such as for functions, it will record the applied to type and enforce it on future invocations.
These applications are considered @tech{negative positions} for @reachin{Refine}.

@subsubsection{Literal values}

@(mint-define! '("true") '("false") '("null"))
@reach{
 10
 0xdeadbeef
 007
 -10
 34.5432
 true
 false
 null
 "reality bytes"
 'it just does' }

A @deftech{literal value},
written @reachin{VALUE},
is an @tech{expression} that evaluates to the given @tech{value}.

The @deftech{null literal} may be written as @reachin{null}.

@deftech{Numeric literal}s may be written in decimal, hexadecimal, or octal.
Numeric literals must obey the @deftech{bit width} of @reachin{UInt} if they are used as @reachin{UInt} values at runtime, but if they only appear at compile-time, then they may be any positive number.
Reach provides abstractions for working with @reachin{Int}s and signed @reachin{FixedPoint} numbers.
@reachin{Int}s may be defined by applying the unary @reachin{+} and @reachin{-} operators to values of type @reachin{UInt}.
Reach provides syntactic sugar for defining signed @reachin{FixedPoint} numbers, in base 10, with decimal syntax.

@deftech{Boolean literal}s may be written as @reachin{true} or @reachin{false}.

@deftech{String literal}s (aka byte strings)
may be written between double or single quotes
(with no distinction between the different styles)
and use the same escaping rules as JavaScript.

@subsubsection{Operator expression}

An @deftech{operator} is a special identifier,
which is either a @tech{unary operator}, or a @tech{binary operator}.

@(hrule)

@(mint-define! '("!") '("-") '("+") '("typeof") '("not") '("minus") '("plus") '("void"))
@reach{
 ! a  // not
 - a  // minus
 + a  // plus
 typeof a
 void a}

A @deftech{unary expression}, written @reachin{UNAOP EXPR_rhs}, where @reachin{EXPR_rhs} is an @tech{expression} and @reachin{UNAOP} is one of the @deftech{unary operator}s: @litchar{! - + typeof void}. All the unary operators, besides @reachin{typeof}, have a
corresponding named version in the standard library.

It is @tech{invalid} to use unary operations on the wrong types of @tech{values}.

When applied to values of type @reachin{UInt}, unary @reachin{-} and @reachin{+} operators will cast
their arguments to type @reachin{Int}. The unary @reachin{-} and @reachin{+} operations are defined for
values of type: @reachin{Int}, and @reachin{FixedPoint}.

@reachin{void a} evaluates to @reachin{null} for all arguments.

@(hrule)

@(mint-define! '("&" "&") '("||") '("+") '("-") '("*") '("/") '("%") '("|") '("&") '("^") '("<" "<") '(">" ">") '("==") '("!=") '("===") '("!==") '(">") '(">" "=") '("<" "=") '("<"))
@reach{
 a && b
 a || b
 a + b
 a - b
 a * b
 a / b
 a % b
 a | b
 a & b
 a ^ b
 a << b
 a >> b
 a == b
 a != b
 a === b
 a !== b
 a > b
 a >= b
 a <= b
 a < b }

@margin-note{Bitwise operations are not supported by all @tech{consensus networks} and greatly decrease the efficiency of verification.}

A @deftech{binary expression}, written @reachin{EXPR_lhs BINOP EXPR_rhs}, where @reachin{EXPR_lhs} and @reachin{EXPR_rhs} are @tech{expressions} and @reachin{BINOP} is one of the @deftech{binary operator}s: @litchar{&& || + - * / % | & ^ << >> == != === !== > >= <= <}.
The operators @reachin{==} (and @reachin{===}) and @reachin{!=} (and @reachin{!==}) operate on all atomic values.
Numeric operations, like @reachin{+} and @reachin{>}, only operate on numbers.
Since all numbers in Reach are integers, operations like @reachin{/} truncate their result.
Boolean operations, like @reachin{&&}, only operate on booleans.
It is @tech{invalid} to use binary operations on the wrong types of @tech{values}.

@(mint-define! '("and") '("or") '("add") '("sub") '("mul") '("div") '("mod") '("lt") '("le") '("ge") '("gt") '("lsh") '("rsh") '("band") '("bior") '("band") '("bxor") '("polyEq") '("polyNeq"))
@reach{
 and(a, b)     // &&
 or(a, b)      // ||
 add(a, b)     // +
 sub(a, b)     // -
 mul(a, b)     // *
 div(a, b)     // /
 mod(a, b)     // %
 lt(a, b)      // <
 le(a, b)      // <=
 ge(a, b)      // >=
 gt(a, b)      // >
 lsh(a, b)     // <<
 rsh(a, b)     // >>
 band(a, b)    // &
 bior(a, b)    // |
 bxor(a, b)    // ^
 polyEq(a, b)  // ==, ===
 polyNeq(a, b) // !=, !==
}

All @tech{binary expression} operators have a corresponding named function in the standard library.
While @reachin{&&} and @reachin{||} may not evaluate their second argument,
their corresponding named functions @reachin{and} and @reachin{or}, always do.

@(mint-define! '("boolEq") '("typeEq") '("intEq") '("digestEq") '("addressEq") '("fxeq") '("ieq"))
@reach{
 polyEq(a, b)    // eq on all types
 boolEq(a, b)    // eq on Bool
 typeEq(a, b)    // eq on types
 intEq(a, b)     // eq on UInt
 digestEq(a, b)  // eq on Digest
 addressEq(a, b) // eq on Addresses
 fxeq(a, b)      // eq on FixedPoint
 ieq(a, b)       // eq on Int
}

@reachin{==} is a function which operates on all types.
Both arguments must be of the same type.
Specialized functions exist for equality checking on each supported type.

@(hrule)

If @reachin{verifyArithmetic} is @reachin{true}, then arithmetic operations automatically make a @tech{static assertion} that their arguments would not overflow the @tech{bit width} of the enable @tech{consensus networks}.
If it is @reachin{false}, then the @tech{connector} will ensure this dynamically.

@subsubsection{xor}

@reach{
 xor(false, false); // false
 xor(false, true);  // true
 xor(true, false);  // true
 xor(true, true);   // false }

@index{xor} @reachin{xor(Bool, Bool)} returns @reachin{true} only when the inputs differ in value.

@subsubsection{Parenthesized expression}

@reach{
 (a + b) - c }

An @tech{expression} may be parenthesized, as in @reachin{(EXPR)}.

@subsubsection[#:tag "ref-programs-tuples"]{Tuples}

@reach{
 [ ]
 [ 1, 2 + 3, 4 * 5 ] }

A @deftech{tuple} literal, written @reachin{[ EXPR_0, ..., EXPR_n ]}, is an @tech{expression} which evaluates to a tuple of @reachin{n} values, where @reachin{EXPR_0} through @reachin{EXPR_n} are @tech{expressions}.

@reachin{...expr} may appear inside tuple expressions, in which case the spreaded expression must evaluate to a tuple or array, which is spliced in place.

@subsubsection[#:tag "ref-programs-arrays"]{@tt{array}}

@(mint-define! '("array"))
@reach{
  const x = array(UInt, [1, 2, 3]); }

Converts a @tech{tuple} of homogeneous values of the specific type into an @deftech{array}.

@subsubsection{Element reference}

@reach{
 arr[3] }

A @deftech{reference}, written @reachin{REF_EXPR[IDX_EXPR]},
where @reachin{REF_EXPR} is an @tech{expression} that evaluates to an @tech{array}, a @tech{tuple}, or a @tech{struct}
and @reachin{IDX_EXPR} is an @tech{expression} that evaluates to a natural number which is less than the size of the array,
selects the element at the given index of the array.
Indices start at zero.

If @reachin{REF_EXPR} is a @tech{tuple}, then @reachin{IDX_EXPR} must be a compile-time constant, because tuples do not support dynamic access, because each element may be a different type.

If @reachin{REF_EXPR} is a @tech{mapping} and @reachin{IDX_EXPR} evaluates to an @tech{address}, then this @tech{reference} evaluates to a value of type @reachin{Maybe(TYPE)}, where @reachin{TYPE} is the @tech{type} of the @tech{mapping}.

@subsubsection{Array & tuple length: @tt{Tuple.length}, @tt{Array.length}, and @tt{.length}}

@(mint-define! '("length"))
@reach{
 Tuple.length(tup);
 tup.length;
 Array.length(arr);
 arr.length; }

@index{Tuple.length} @reachin{Tuple.length} Returns the length of the given tuple.

@index{Array.length} @reachin{Array.length} Returns the length of the given array.

Both may be abbreviated as @reachin{expr.length} where @reachin{expr} evaluates to a tuple or an array.

@subsubsection{Array & tuple update: @tt{Tuple.set}, @tt{Array.set}, and @tt{.set}}

@(mint-define! '("set"))
@reach{
 Tuple.set(tup, idx, val);
 tup.set(idx, val);
 Array.set(arr, idx, val);
 arr.set(idx, val); }

@index{Tuple.set} @reachin{Tuple.set} Returns a new @tech{tuple} identical to @reachin{tup},
except that index @reachin{idx} is replaced with @reachin{val}.
The @reachin{idx} must be a compile-time constant, because tuples do not support dynamic access, because each element may be a different type.

@index{Array.set} @reachin{Array.set} Returns a new @tech{array} identical to @reachin{arr}, except that index @reachin{idx} is replaced with @reachin{val}.

Both may be abbreviated as @reachin{expr.set(idx, val)} where @reachin{expr} evaluates to a @tech{tuple} or an @tech{array}.

@subsubsection{Foldable operations}

The following methods are available on any @(mint-define! '("Foldable"))@reachin{Foldable} containers, such as: @reachin{Array}s and @reachin{Map}s.

@subsubsub*section{ @tt{Foldable.forEach} && @tt{.forEach}}

@(mint-define! '("forEach"))
@reach{
 c.forEach(f)
 Foldable.forEach(c, f)
 Array.forEach(c, f)
 Map.forEach(c, f) }

@index{Foldable.forEach} @reachin{Foldable.forEach(c, f)} iterates the function @reachin{f} over the elements of a container @reachin{c}, discarding the result.
This may be abbreviated as @reachin{c.forEach(f)}.

@subsubsub*section{@tt{Foldable.all} && @tt{.all}}

@(mint-define! '("all"))
@reach{
  Foldable.all(c, f)
  Array.all(c, f)
  Map.all(c, f)
  c.all(f) }

@index{Foldable.all} @reachin{Foldable.all(c, f)} determines whether the predicate, @tt{f}, is satisfied
by every element of the container, @tt{c}.

@subsubsub*section{@tt{Foldable.any} && @tt{.any}}

@(mint-define! '("any"))
@reach{
  Foldable.any(c, f)
  Array.any(c, f)
  Map.any(c, f)
  c.any(f) }

@index{Foldable.any} @reachin{Foldable.any(c, f)} determines whether the predicate, @tt{f}, is satisfied
by at least one element of the container, @tt{c}.

@subsubsub*section{@tt{Foldable.or} && @tt{.or}}

@(mint-define! '("or"))
@reach{
  Foldable.or(c)
  Array.or(c)
  Map.or(c)
  c.or() }

@index{Foldable.or} @reachin{Foldable.or(c)} returns the disjunction of a container of @reachin{Bool}s.

@subsubsub*section{@tt{Foldable.and} && @tt{.and}}

@(mint-define! '("and"))
@reach{
  Foldable.and(c)
  Array.and(c)
  Map.and(c)
  c.and() }

@index{Foldable.and} @reachin{Foldable.and(c)} returns the conjunction of a container of @reachin{Bool}s.

@subsubsub*section{@tt{Foldable.includes} && @tt{.includes}}

@(mint-define! '("includes"))
@reach{
  Foldable.includes(c, x)
  Array.includes(c, x)
  Map.includes(c, x)
  c.includes(x) }

@index{Foldable.includes} @reachin{Foldable.includes(c, x)} determines whether the container includes
the element, @tt{x}.

@subsubsub*section{@tt{Foldable.count} && @tt{.count}}

@(mint-define! '("count"))
@reach{
  Foldable.count(c, f)
  Array.count(c, f)
  Map.count(c, f)
  c.count(f) }

@index{Foldable.count} @reachin{Foldable.count(c, f)} returns the number of elements in @tt{c} that
satisfy the predicate, @tt{f}.

@subsubsub*section{@tt{Foldable.size} && @tt{.size}}

@(mint-define! '("size"))
@reach{
  Foldable.size(c)
  Array.size(c)
  Map.size(c)
  c.size() }

@index{Foldable.size} @reachin{Foldable.size(c)} returns the number of elements in @tt{c}.

@subsubsub*section{@tt{Foldable.min} && @tt{.min}}

@(mint-define! '("min"))
@reach{
  Foldable.min(c)
  Array.min(c)
  Map.min(c)
  c.min() }

@index{Foldable.min} @reachin{Foldable.min(arr)} returns the lowest number in a container of @tt{UInt}s.

@subsubsub*section{@tt{Foldable.max} && @tt{.max}}

@(mint-define! '("max"))
@reach{
  Foldable.max(c)
  Array.max(c)
  Map.max(c)
  c.max() }

@index{Foldable.max} @reachin{Foldable.max(c)} returns the largest number in a container of @tt{UInt}s.

@subsubsub*section{@tt{Foldable.sum} && @tt{.sum}}

@(mint-define! '("sum"))
@reach{
  Foldable.sum(c)
  Array.sum(c)
  Map.sum(c)
  c.sum() }

@index{Foldable.sum} @reachin{Foldable.sum(c)} returns the sum of a container of @tt{UInt}s.

@subsubsub*section{@tt{Foldable.product} && @tt{.product}}

@(mint-define! '("product"))
@reach{
  Foldable.product(c)
  Array.product(c)
  Map.product(c)
  c.product() }

@index{Foldable.product} @reachin{Foldable.product(c)} returns the product of a container of @tt{UInt}s.

@subsubsub*section{@tt{Foldable.average} && @tt{.average}}

@(mint-define! '("average"))
@reach{
  Foldable.average(c)
  Array.average(c)
  Map.average(c)
  c.average() }

@index{Foldable.average} @reachin{Foldable.average(c)} returns the mean of a container of @tt{UInt}s.

@subsubsection{Array group operations}

@reachin{Array} is a @reachin{Foldable} container. Along with the methods of @reachin{Foldable}, the
following methods may be used with @reachin{Array}s.

@subsubsub*section{@tt{Array.iota}}

@(mint-define! '("iota"))
@reach{
 Array.iota(5) }

@index{Array.iota} @reachin{Array.iota(len)} returns an array of length @reachin{len}, where each element is the same as its index.
For example, @reachin{Array.iota(4)} returns @reachin{[0, 1, 2, 3]}.
The given @reachin{len} must evaluate to an integer at compile-time.

@subsubsub*section{@tt{Array.replicate} && @tt{.replicate}}

@(mint-define! '("Array_replicate") '("replicate"))
@reach{
 Array.replicate(5, "five")
 Array_replicate(5, "five") }

@index{Array.replicate} @reachin{Array.replicate(len, val)} returns an array of length @reachin{len}, where each element is @reachin{val}.
For example, @reachin{Array.replicate(4, "four")} returns @reachin{["four", "four", "four", "four"]}.
The given @reachin{len} must evaluate to an integer at compile-time.

@subsubsub*section{@tt{Array.concat} && @tt{.concat}}

@(mint-define! '("concat"))
@reach{
 Array.concat(x, y)
 x.concat(y) }

@index{Array.concat} @reachin{Array.concat(x, y)} concatenates the two arrays @reachin{x} and @reachin{y}.
This may be abbreviated as @reachin{x.concat(y)}.

@subsubsub*section{@tt{Array.empty}}

@(mint-define! '("Array_empty") '("empty"))
@reach{
 Array_empty
 Array.empty }

@index{Array.empty} @reachin{Array.empty} is an array with no elements.
It is the identity element of @reachin{Array.concat}.
It may also be written @reachin{Array_empty}.

@subsubsub*section{@tt{Array.zip} && @tt{.zip}}

@(mint-define! '("zip"))
@reach{
 Array.zip(x, y)
 x.zip(y) }

@index{Array.zip} @reachin{Array.zip(x, y)} returns a new array the same size as @reachin{x} and @reachin{y} (which must be the same size) whose elements are tuples of the elements of @reachin{x} and @reachin{y}.
This may be abbreviated as @reachin{x.zip(y)}.

@subsubsub*section{@tt{Array.map} && @tt{.map}}

@(mint-define! '("map"))
@reach{
 Array.map(arr, f)
 arr.map(f) }

@index{Array.map} @reachin{Array.map(arr, f)} returns a new array, @reachin{arr_mapped}, the same size as @reachin{arr}, where @reachin{arr_mapped[i] = f(arr[i])} for all @reachin{i}.
For example, @reachin{Array.iota(4).map(x => x+1)} returns @reachin{[1, 2, 3, 4]}.
This may be abbreviated as @reachin{arr.map(f)}.

This function is generalized to an arbitrary number of arrays of the same size, which are provided before the @reachin{f} argument.
For example, @reachin{Array.iota(4).map(Array.iota(4), add)} returns @reachin{[0, 2, 4, 6]}.

@subsubsub*section{@tt{Array.reduce} && @tt{.reduce}}

@(mint-define! '("reduce"))
@reach{
 Array.reduce(arr, z, f)
 arr.reduce(z, f) }

@index{Array.reduce} @reachin{Array.reduce(arr, z, f)} returns the @link["https://en.wikipedia.org/wiki/Fold_(higher-order_function)"]{left fold} of the function @reachin{f} over the given array with the initial value @reachin{z}.
For example, @reachin{Array.iota(4).reduce(0, add)} returns @reachin{((0 + 1) + 2) + 3 = 6}.
This may be abbreviated as @reachin{arr.reduce(z, f)}.

This function is generalized to an arbitrary number of arrays of the same size, which are provided before the @reachin{z} argument.
For example, @reachin{Array.iota(4).reduce(Array.iota(4), 0, (x, y, z) => (z + x + y))} returns @reachin{((((0 + 0 + 0) + 1 + 1) + 2 + 2) + 3 + 3)}.

@subsubsub*section{@tt{Array.indexOf} && @tt{.indexOf}}

@(mint-define! '("indexOf"))
@reach{
  Array.indexOf(arr, x)
  arr.indexOf(x) }

@index{Array.indexOf} @reachin{Array.indexOf(arr, x)} returns the index of the first element
in the given array that is equal to @tt{x}. The return value is of type @reachin{Maybe(UInt)}. If
the value is not present in the array, @reachin{None} is returned.

@subsubsub*section{@tt{Array.findIndex} && @tt{.findIndex}}

@(mint-define! '("findIndex"))
@reach{
  Array.findIndex(arr, f)
  arr.findIndex(f) }

@index{Array.findIndex} @reachin{Array.findIndex(arr, f)} returns the index of the first element
in the given array that satisfies the predicate @tt{f}. The return value is of type @reachin{Maybe(UInt)}. If
no value in the array satisfies the predicate, @reachin{None} is returned.

@subsubsection{Mapping group operations}

@reachin{Map} is a @reachin{Foldable} container. @tech{Mappings} may be aggregated with the following
operations and those of @reachin{Foldable} within the @reachin{invariant} of a @reachin{while} loop.

@subsubsub*section{@tt{Map.reduce} && @tt{.reduce}}

@reach{
 Map.reduce(map, z, f)
 map.reduce(z, f) }

@index{Map.reduce} @reachin{Map.reduce(map, z, f)} returns the @link["https://en.wikipedia.org/wiki/Fold_(higher-order_function)"]{left fold} of the function @reachin{f} over the given @tech{mapping} with the initial value @reachin{z}.
For example, @reachin{m.reduce(0, add)} sums the elements of the @tech{mapping}.
This may be abbreviated as @reachin{map.reduce(z, f)}.

The function @reachin{f} must satisfy the property, for all @reachin{z}, @reachin{a}, @reachin{b}, @reachin{f(f(z, b), a) == f(f(z, a), b)}, because the order of evaluation is unpredictable.

@subsubsection[#:tag "ref-programs-objects"]{Objects}

@reach{
  { }
  { x: 3, "yo-yo": 4 }
  { [1 < 2 ? "one" : "two"]: 5 }
}

An @deftech{object},
typically written @reachin{{ KEY_0: EXPR_0, ..., KEY_n: EXPR_n }},
where @reachin{KEY_0} through @reachin{KEY_n} are @tech{identifiers} or @tech{string literal}s
and @reachin{EXPR_0} through @reachin{EXPR_n} are @tech{expressions},
is an @tech{expression} which evaluates to an @tech{object}
with fields @reachin{KEY_0} through @reachin{KEY_n}.

Additional object literal syntax exists for convenience, such as:

@reach{
  { ...obj, z: 5 }
}

An @deftech{object splice},
where all fields from @reachin{obj} are copied into the object;
these fields may be accompanied by additional fields specified afterwards.

@reach{
  { x, z: 5 }
}

Shorthand for @reachin{{ x: x, z: 5}}, where @reachin{x} is any @tech{bound identifier}.

@subsubsection[#:tag "ref-programs-structs"]{Structs}

@reach{
  const Posn = Struct([["x", UInt], ["y", UInt]]);
  const p1 = Posn.fromObject({x: 1, y: 2});
  const p2 = Posn.fromTuple([1, 2]);
}

A @deftech{struct} is a combination of a @tech{tuple} and an @tech{object}.
It has named elements, like an @tech{object}, but is ordered like a @tech{tuple}, so its elements may be accessed by either name or position.
@tech{Structs} exist for interfacing with non-Reach @tech{remote objects}, where both parties must agree to the runtime representation of the values.

@index{Struct.fromTuple}
A @tech{struct} instance may be constructed by calling the @reachin{fromTuple} method of a @tech{struct} type instance (like @reachin{Posn}) with a @tech{tuple} of the appropriate length.

@index{Struct.fromObject}
A @tech{struct} instance may be constructed by calling the @reachin{fromObject} method of a @tech{struct} type instance (like @reachin{Posn}) with an @tech{object} with the appropriate fields.

@index{Struct.toTuple}
@index{Struct.toObject}
Structs may be converted into a corresponding @tech{tuple} or @tech{object} via the @reachin{toTuple} and @reachin{toObject} methods on the @reachin{Struct} value (as well as @tech{struct} type instances, like @reachin{Posn} in the example above):

@reach{
  assert(Posn.toTuple(p1)[0] == 1);
  assert(Struct.toObject(p2).y == 2);
}

@subsubsection{Field reference}

@reach{
  obj.x
}

An @deftech{object reference},
written @reachin{OBJ.FIELD},
where @reachin{OBJ} is an expression that evaluates to an @tech{object} or a @tech{struct},
and @reachin{FIELD} is a @tech{valid} @tech{identifier},
accesses the @litchar{FIELD} @deftech{field} of object OBJ.

@subsubsection{@tt{Object.set}}

@(mint-define! '("Object_set"))
@reach{
 Object.set(obj, fld, val);
 Object_set(obj, fld, val);
 { ...obj, [fld]: val };
}

@index{Object.set} Returns a new @tech{object} identical to @reachin{obj},
except that field @reachin{fld} is replaced with @reachin{val}.

@subsubsection{@tt{Object.setIfUnset}}

@(mint-define! '("Object_setIfUnset"))
@reach{
 Object.setIfUnset(obj, fld, val);
 Object_setIfUnset(obj, fld, val);
}

@index{Object.setIfUnset} Returns a new object identical to @reachin{obj},
except that field @reachin{fld} is @reachin{val} if @reachin{fld} is not already present in @reachin{obj}.

@subsubsection{@tt{Object.has}}

@reach{
 Object.has(obj, fld);
}

@index{Object.has} Returns a boolean indicating whether the @tech{object} has the field @reachin{fld}.
This is statically known.

@subsubsection[#:tag "ref-programs-data"]{Data}

@reach{
 const Taste = Data({Salty: Null,
                     Spicy: Null,
                     Sweet: Null,
                     Umami: Null});
 const burger = Taste.Umami();

 const Shape = Data({ Circle: Object({r: UInt}),
                      Square: Object({s: UInt}),
                      Rect: Object({w: UInt, h: UInt}) });
 const nice = Shape.Circle({r: 5}); }

A @deftech{data instance} is written @reachin{DATA.VARIANT(VALUE)}, where @reachin{DATA} is @reachin{Data} type, @reachin{VARIANT} is the name of one of @reachin{DATA}'s variants, and @reachin{VALUE} is a value matching the type of the variant.
As a special case, when the type of a variant is @reachin{Null}, the @reachin{VALUE} may be omitted, as shown in the definition of @reachin{burger} in the same above.

@tech{Data instances} are consumed by @reachin{switch} statements.

@subsubsection{@tt{Maybe}}

@(mint-define! '("Maybe") '("Some") '("None") '("fromMaybe"))
@reach{
 const MayInt = Maybe(UInt);
 const bidA = MayInt.Some(42);
 const bidB = MayInt.None(null);

 const getBid = (m) => fromMaybe(m, (() => 0), ((x) => x));
 const bidSum = getBid(bidA) + getBid(bidB);
 assert(bidSum == 42); }

@link["https://en.wikipedia.org/wiki/Option_type"]{Option types} are represented in Reach through the built-in @reachin{Data} type, @reachin{Maybe}, which has two variants: @reachin{Some} and @reachin{None}.

@reachin{Maybe} is defined by
@reach{
 export const Maybe = (A) => Data({None: Null, Some: A}); }

This means it is a function that returns a @reachin{Data} type specialized to a particular type in the @reachin{Some} variant.

@reachin{Maybe} instances can be conveniently consumed by @reachin{fromMaybe(mValue, onNone, onSome)}, where @reachin{onNone} is a function of no arguments which is called when @reachin{mValue} is @reachin{None}, @reachin{onSome} is a function of on argument which is called with the value when @reachin{mValue} is @reachin{Some}, and @reachin{mValue} is a @tech{data instance} of @reachin{Maybe}.

@(mint-define! '("isNone") '("isSome"))
@reach{
  const m = Maybe(UInt).Some(5);
  isNone(m); // false
  isSome(m); // true
}

@index{isNone} @reachin{isNone} is a convenience method that determines whether the variant is @tt{isNone}.

@index{isSome} @reachin{isSome} is a convenience method that determines whether the variant is @tt{isSome}.


@(mint-define! '("fromSome"))
@reach{
  fromSome(Maybe(UInt).Some(1), 0); // 1
  fromSome(Maybe(UInt).None(), 0);  // 0
}

@index{fromSome} @reachin{fromSome} receives a @reachin{Maybe} value and a default value as arguments and will return the value inside
of the @reachin{Some} variant or the default value otherwise.

@(mint-define! '("maybe"))
@reach{
  const add1 = (x) => x + 1;
  maybe(Maybe(UInt).Some(1), 0, add1); // 2
  maybe(Maybe(UInt).None(), 0, add1);  // 0
}

@index{maybe} @reachin{maybe(m, defaultVal, f)} receives a @reachin{Maybe} value, a default value, and a unary function as arguments. The function will
either return the application of the function, @reachin{f}, to the @reachin{Some} value or return the default value provided.

@subsubsection{@tt{Either}}

@reachin{Either} is defined by
@reach{
  export const Either = (A, B) => Data({Left: A, Right: B}); }

@reachin{Either} can be used to represent values with two possible types.

Similar to @tt{Maybe}, @tt{Either} may be used to represent values that are correct or erroneous.
A successful result is stored, by convention, in @tt{Right}. Unlike @tt{None}, @tt{Left} may
carry additional information about the error.

@(mint-define! '("either"))
@reach{
  either(e, onLeft, onRight) }

@index{either} @reachin{either(e, onLeft, onRight)} For an @tt{Either} value, @tt{e}, @tt{either}
will either apply the function @tt{onLeft} or @tt{onRight} to the appropriate variant value.

@(mint-define! '("isLeft") '("isRight") '("fromLeft") '("fromRight"))
@reach{
  const e = Either(UInt, Bool);
  const l = e.Left(1);
  const r = e.Right(true);
  isLeft(l);  // true
  isRight(l); // false
  const x = fromLeft(l, 0);      // x = 1
  const y = fromRight(l, false); // y = false }

@index{isLeft} @reachin{isLeft} is a convenience method that determines whether the variant is @tt{Left}.

@index{isRight} @reachin{isRight} is a convenience method that determines whether the variant is @tt{Right}.

@index{fromLeft} @reachin{fromLeft(e, default)} is a convenience method that returns the value in @tt{Left},
or @tt{default} if the variant is @tt{Right}.

@index{fromRight} @reachin{fromRight(e, default)} is a convenience method that returns the value in @tt{Right},
or @tt{default} if the variant is @tt{Left}.

@subsubsection{@tt{match}}

@(mint-define! '("match"))
@reach{
 const Value = Data({
    EBool: Bool,
    EInt: UInt,
    ENull: Null,
  });
  const v1 = Value.EBool(true);
  const v2 = Value.EInt(200);
  const isTruthy = (v) =>
    v.match({
      EBool: (b) => { return b },
      EInt : (i) => { return i != 0 },
      ENull: ()  => { return false }
    });

  assert(isTruthy(v1));
  assert(isTruthy(v2));
}

A @deftech{match expression}, written @reachin{VAR.match({ CASE ... })}, where @tt{VAR} is a variable
bound to a @tech{data instance} and @tt{CASE} is @tt{VARIANT: FUNCTION}, where @tt{VARIANT} is a
variant or @reachin{default}, and @tt{FUNCTION} is a function that takes the same arguments as the
variant constructor.
If the variant has a type of @reachin{Null}, then the function is allowed to take no arguments.
@reachin{default} functions must always take an argument, even if all defaulted variants have type @reachin{Null}.

@reachin{match} is similar to a @tech{switch statement}, but since it is an expression, it
can be conveniently used in places like the right hand side of an assignment statement.

Similar to a @tech{switch statement}, the cases are expected to be exhaustive and nonredundant,
all cases have empty @@tech{tails}, and it may only include a @tech{consensus transfer} in
its cases if it is within a @tech{consensus step}.

@subsubsection{Conditional expression}

@(mint-define! '("?"))
@reach{
 choosesFirst ? [ heap1 - amount, heap2 ] : [ heap1, heap2 - amount ] }

A @deftech{conditional expression}, written @reachin{COND_E ? NOT_FALSE_E : FALSE_E}, where @reachin{COND_E}, @reachin{NOT_FALSE_E}, and @reachin{FALSE_E} are @tech{expressions}, selects between the @tech{values} which @reachin{NOT_FALSE_E} and @reachin{FALSE_E} evaluate to based on whether @reachin{COND_E} evaluates to @reachin{false}.

@(mint-define! '("ite"))
@reach{
 ite(choosesFirst, [heap1 - amount, heap2], [heap1, heap2 - amount])
}

@tech{Conditional expression}s may also be written with the @reachin{ite} function,
however, note that this function always evaluates both of its branches,
while the regular conditional expression only evaluates one branch.

@subsubsection{Arrow expression}

@(mint-define! '("=" ">"))
@reach{
 (() => 4)
 ((x) => x + 1)
 ((x) => { const y = x + 1;
           return y + 1; })
 ((x, y) => { assert(x + y == 3); })(1, 2);
 ((x, y) => { assert(x + y == 3); })(...[1, 2]);
 ((x, y = 2) => { assert(x + y == 3); })(1);
 ((x, y = 2) => { assert(x + y == 2); })(1, 1);
 (([x, y]) => { assert(x + y == 3); })([1, 2]);
 (({x, y}) => { assert(x + y == 3); })({x: 1, y: 2});
 (([x, [y]]) => { assert(x + y == 3); })([1,[2]]);
 (([x, {y}]) => { assert(x + y == 3); })([1,{ y: 2 }]);
 ((...xs) => Foldable.sum(xs))(1, 2, 3)
}

An @deftech{arrow expression}, written @reachin{(LHS_0, ..., LHS_n) => EXPR}, where @reachin{LHS_0} through @reachin{LHS_n} are left-hand sides and @reachin{EXPR} is an @tech{expression}, evaluates to an function which is an abstraction of @reachin{EXPR} over @reachin{n} values compatible with the respective left-hand side.
Like @tech{function definition}s, @tech{arrow expression}s may use default argument notation and @tech{rest parameter}s.

@subsubsection{@tt{makeEnum}}

@(mint-define! '("makeEnum"))
@reach{
  const [ isHand, ROCK, PAPER, SCISSORS ] = makeEnum(3); }

An @deftech{enumeration} (or @deftech{enum}, for short),
can be created by calling the @reachin{makeEnum} function, as in @reachin{makeEnum(N)},
where @reachin{N} is the number of distinct values in the enum.
This produces a tuple of @reachin{N+1} values,
where the first value is a @reachin{Fun([UInt], Bool)}
which tells you if its argument is one of the enum's values,
and the next N values are distinct @reachin{UInt}s.

@subsubsection{@tt{assert}}

@(mint-define! '("assert"))
@reach{
 assert( claim, [msg] ) }

@index{assert} A @tech{static assertion} which is only @tech{valid} if @reachin{claim} always evaluates to @reachin{true}.
@margin-note{The Reach compiler will produce a counter-example (i.e. an assignment of the identifiers in the program to falsify the @reachin{claim}) when an @tech{invalid} @reachin{claim} is provided.
It is possible to write a @reachin{claim} that actually always evaluates to @reachin{true}, but for which our current approach cannot prove always evaluates to @reachin{true}; if this is the case, Reach will fail to compile the program, reporting that its analysis is incomplete.
Reach will never produce an erroneous counter-example.}
It accepts an optional bytes argument, which is included in any reported violation.

@margin-note{See @seclink["guide-assert"]{the guide section on verification} to better understand how and what to verify in your program.}

@subsubsection{@tt{forall}}

@(mint-define! '("forall"))
@reach{
 forall( Type )
 forall( Type, (var) => BLOCK ) }

@index{forall} The single argument version returns an abstract value of the given type.
It may only be referenced inside of @tech{assert}ions; any other reference is invalid.

The two argument version is an abbreviation of calling the second argument with the result of @reachin{forall(Type)}.
This is convenient for writing general claims about expressions, such as

@reach{
 forall(UInt, (x) => assert(x == x)); }

@subsubsection{@tt{possible}}

@(mint-define! '("possible"))
@reach{
 possible( claim, [msg] ) }

@index{possible} A @tech{possibility assertion} which is only @tech{valid} if it is possible for @reachin{claim} to evaluate to @reachin{true} with @tech{honest} @tech{frontends} and @tech{participants}.
It accepts an optional bytes argument, which is included in any reported violation.

@subsubsection{@tt{digest}}

@(mint-define! '("digest"))
@reach{
 digest( arg_0, ..., arg_n ) }

The @tech{digest} primitive performs a @link["https://en.wikipedia.org/wiki/Cryptographic_hash_function"]{cryptographic hash} of the binary encoding of the given arguments.
This returns a @reachin{Digest} value.
The exact algorithm used depends on the @tech{connector}.

@subsubsection{@tt{balance}}

@(mint-define! '("balance"))
@reach{
 balance();
 balance(gil); }

The @deftech{balance} primitive returns the balance of the @tech{contract} @tech{account} for the @|DApp|.
It takes an optional @tech{non-network token} value, in which case it returns the balance of the given token.

@subsubsection{@tt{lastConsensusTime}}

@(mint-define! '("lastConsensusTime"))
@reach{
 lastConsensusTime() }

The @deftech{lastConsensusTime} primitive returns the @tech{time} of the last @tech{publication} of the @|DApp|.
This may not be available if there was no such previous publication, such as at the beginning of an application where @reachin{deployMode} is @reachin{'firstMsg'}.

@margin-note{Why is there no @tt{thisConsensusTime}?
Some networks do not support observing the time of a consensus operation until after it has finalized.
This aides scalability, because it increases the number of times when an operation could be finalized.}

@subsubsection{@tt{makeDeadline}}

@(mint-define! '("makeDeadline"))
@reach{
  const [ timeRemaining, keepGoing ] = makeDeadline(10); }

@index{makeDeadline} @reachin{makeDeadline(deadline)} takes an @reachin{UInt} as an argument and returns a pair of functions
that can be used for dealing with absolute deadlines. It internally determines the end time based off of the deadline
and the last consensus time—at the time of calling @reachin{makeDeadline}. @tt{timeRemaining} will calculate the difference
between the end time and the current last consensus time. @tt{keepGoing} determines whether the current last consensus time
is less than the end time. It is typical to use the two fields for the @tt{while} and @tt{timeout} field of a @reachin{parallelReduce}
expression. For example:

@reach{
  const [ timeRemaining, keepGoing ] = makeDeadline(10);
  const _ = parallelReduce(...)
    .invariant(...)
    .while( keepGoing() )
    .case(...)
    .timeout( timeRemaining(), () => { ... }) }

This pattern is so common that it can be abbreviated as @reachin{.timeRemaining}.


@subsubsection{@tt{implies}}

@(mint-define! '("implies"))
@reach{
 implies( x, y ) }

@index{implies} Returns @reachin{true} if @reachin{x} is @reachin{false} or @reachin{y} is @reachin{true}.

@subsubsection{@tt{ensure}}

@(mint-define! '("ensure"))
@reach{
 ensure( pred, x ) }

@index{ensure} Makes a @tech{static assertion} that @reachin{pred(x)} is @reachin{true} and returns @reachin{x}.

@subsubsection{@tt{hasRandom}}

@(mint-define! '("hasRandom"))
@reach{
 hasRandom }

@index{hasRandom} A @tech{participant interact interface} which specifies @litchar{random} as a function that takes no arguments and returns an unsigned integer of @tech{bit width} bits.


@subsubsection{@tt{compose}}

@(mint-define! '("compose"))
@reach{
 compose(f, g) }

@index{compose} Creates a new function that applies it's argument to @tt{g}, then pipes the result to the function @tt{f}.
The argument type of @tt{f} must be the return type of @tt{g}.


@subsubsection{@tt{sqrt}}

@(mint-define! '("sqrt"))
@reach{
  sqrt(81, 10) }

@index{sqrt} Calculates an approximate square root of the first argument. This method utilizes
the @link["https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method"]{Babylonian Method} for computing
the square root. The second argument must be an @reachin{UInt} whose value is known at compile time, which represents the number
of iterations the algorithm should perform.

For reference, when performing @reachin{5} iterations, the algorithm can reliably calculate the square root
up to @tt{32} squared, or @tt{1,024}. When performing @reachin{10} iterations, the algorithm can reliably calculate the
square root up to @tt{580} squared, or @tt{336,400}.

@subsubsection{@tt{pow}}

@(mint-define! '("pow"))
@reach{
  pow (2, 40, 10) // => 1,099,511,627,776 }

@index{pow} @reachin{pow(base, power, precision)} Calculates the approximate value of raising base to power.
The third argument must be an @reachin{UInt} whose value is known at compile time, which represents the number
of iterations the algorithm should perform. For reference, @tt{6} iterations provides enough accuracy to calculate
up to @tt{2^64 - 1}, so the largest power it can compute is @tt{63}.

@subsubsection{Signed Integers}

The standard library provides abstractions for dealing with signed integers. The following definitions
are used to represent @reachin{Int}s:

@margin-note{
  @tt{Int} is represented as an object, as opposed to a scalar value, because some platforms
  that Reach targets do not provide native support for signed integers. }

@(mint-define! '("Int") '("Pos") '("Neg"))
@reach{
  const Int = { sign: bool, i: UInt };
  const Pos = true;
  const Neg = false;  }

@index{int} @reachin{int(Bool, UInt)} is shorthand for defining an @reachin{Int} record. You may also
use the @reachin{+} and @reachin{-} unary operators to declare integers instead of @reachin{UInt}s.

@(mint-define! '("int"))
@reach{
  int(Pos, 4); // represents 4
  int(Neg, 4); // represents -4
  -4;          // represents -4
  +4;          // represents 4 : Int
   4;          // represents 4 : UInt }

@index{iadd} @reachin{iadd(x, y)} adds the @reachin{Int} @tt{x} and the @reachin{Int} @tt{y}.

@index{isub} @reachin{isub(x, y)} subtracts the @reachin{Int} @tt{y} from the @reachin{Int} @tt{x}.

@index{imul} @reachin{imul(x, y)} multiplies the @reachin{Int} @tt{x} and the @reachin{Int} @tt{y}.

@index{idiv} @reachin{idiv(x, y)} divides the @reachin{Int} @tt{x} by the @reachin{Int} @tt{y}.

@index{imod} @reachin{imod(x, y)} finds the remainder of dividing the @reachin{Int} @tt{x} by the @reachin{Int} @tt{y}.

@index{ilt} @reachin{ilt(x, y)} determines whether @tt{x} is less than @tt{y}.

@index{ile} @reachin{ile(x, y)} determines whether @tt{x} is less than or equal to @tt{y}.

@index{igt} @reachin{igt(x, y)} determines whether @tt{x} is greather than @tt{y}.

@index{ige} @reachin{ige(x, y)} determines whether @tt{x} is greater than or equal to @tt{y}.

@index{ieq} @reachin{ieq(x, y)} determines whether @tt{x} is equal to @tt{y}.

@index{ine} @reachin{ine(x, y)} determines whether @tt{x} is not equal to @tt{y}.

@index{imax} @reachin{imax(x, y)} returns the larger of two @reachin{Int}s.

@index{abs} @reachin{abs(i)} returns the absolute value of an @reachin{Int}. The return value is of type @reachin{UInt}.

@subsubsection{Fixed-Point Numbers}

@reachin{FixedPoint} is defined by

@(mint-define! '("FixedPoint"))
@reach{
  export const FixedPoint = Object({ sign: bool, i: Object({ scale: UInt, i: UInt }) }); }

@reachin{FixedPoint} can be used to represent numbers with a fixed number of digits after the decimal point.
They are handy for representing fractional values, especially in base 10. The value of a fixed point number is determined
by dividing the underlying integer value, @tt{i}, by its scale factor, @tt{scale}. For example, we could
represent the value @reachin{1.234} with @reachin{{ sign: Pos, i: { scale: 1000, i : 1234 } }} or @reachin{fx(1000)(Pos, 1234)}.
Alternatively, Reach provides syntactic sugar for defining @reachin{FixedPoint} numbers. One can simply write
@reachin{1.234}, which will assume the value is in base 10. A scale factor of @tt{1000} correlates to 3 decimal
places of precision. Similarly, a scale factor of @tt{100} would have 2 decimal places of precision.

@(mint-define! '("fx"))
@reach{
  const scale = 10;
  const i = 56;
  fx(scale)(Neg, i); // represents - 5.6 }

@index{fx} @reachin{fx(scale)(i)} will return a function that can be used to
instantiate fixed point numbers with a particular scale factor.

@(mint-define! '("fxint"))
@reach{
  const i = 4;
  fxint(-i); // represents - 4.0 }

@index{fxint} @reachin{fxint(Int)} will cast the @reachin{Int} arg as a @reachin{FixedPoint}
number with a @tt{scale} of 1.

@(mint-define! '("fxrescale"))
@reach{
  const x = fx(1000)(Pos, 1234); // x = 1.234
  fxrescale(x, 100);    // => 1.23 }

@index{fxrescale} @reachin{fxrescale(x, scale)} will convert a fixed point number from using
one scale to another. This operation can result in loss of precision, as demonstrated in the above example.

@(mint-define! '("fxunify"))
@reach{
  const x = fx(1000)(Pos, 824345); // x = 824.345
  const y = 45.67;
  fxunify(x, y);    // => [ 1000, 824.345, 45.670 ] }

@index{fxunify} @reachin{fxunify(x, y)} will convert the fixed point numbers
to use the same scale. The larger scale of the two arguments will be chosen. The function will return a @tt{3-tuple} consisting
of the common scale and the newly scaled values.

@index{fxadd} @reachin{fxadd(x, y)} adds two fixed point numbers.

@index{fxsub} @reachin{fxsub(x, y)} subtracts two fixed point numbers.

@index{fxmul} @reachin{fxmul(x, y)} multiplies two fixed point numbers.

@(mint-define! '("fxdiv"))
@reach{
  fxdiv(34.56, 1.234, 10)     // => 28
  fxdiv(34.56, 1.234, 100000) // => 28.0064 }

@index{fxdiv} @reachin{fxdiv(x, y, scale_factor)} divides two fixed point numbers. The numerator, @tt{x},
will be multiplied by the scale factor to provide a more precise answer. For example,

@index{fxmod} @reachin{fxmod(x, y)} finds the remainder of dividing @tt{x} by @tt{y}.

@index{fxfloor} @reachin{fxfloor(x)} returns the greatest integer not greater than @tt{x}.

@index{fxsqrt} @reachin{fxsqrt(x, k)} approximates the sqrt of the fixed number, @tt{x}, using
@tt{k} iterations of the @reachin{sqrt} algorithm.

@(mint-define! '("fxpow"))
@reachin{
  const base  = 2.0;
  const power = 0.33;
  fxpow(base, power, 10, 1000);    // 1.260
  fxpow(base, power, 10, 10000);   // 1.2599
  fxpow(base, power, 10, 1000000); // 1.259921 }

@index{fxpow} @reachin{fxpow(base, power, precision, scalePrecision)} approximates the power of the fixed number, @tt{base},
raised to the fixed point number, @tt{power}. The third argument must be an @reachin{UInt} whose value is known
at compile time, which represents the number of iterations the algorithm should perform.
The @tt{scalePrecision} argument must be a @tt{UInt} and represents the scale of the return value. Choosing a larger
@tt{scalePrecision} allows for more precision when approximating the power, as demonstrated in the example below:

@index{fxpowi} @reachin{fxpowi(base, power, precision)} approximates the power of the fixed number, @tt{base},
raised to the @reachin{Int}, @tt{power}. The third argument must be an @reachin{UInt} whose value is known
at compile time, which represents the number of iterations the algorithm should perform. For reference, @tt{6} iterations
provides enough accuracy to calculate up to @tt{2^64 - 1}, so the largest power it can compute is @tt{63}.

@(mint-define! '("fxpowui"))
@reachin{
  fxpowui(5.8, 3, 10); // 195.112 }

@index{fxpowui} @reachin{fxpowui(base, power, precision)} approximates the power of
the fixed number, @tt{base}, raised to the @reachin{UInt}, @tt{power}. The third
argument must be an @reachin{UInt} whose value is known at compile time.

@index{fxcmp} @reachin{fxcmp(op, x, y)} applies the comparison
operator to the two fixed point numbers after unifying their scales.

There are convenience methods defined for comparing fixed point numbers:

@index{fxlt} @reachin{fxlt(x, y)} tests whether @tt{x} is less than @tt{y}.

@index{fxle} @reachin{fxle(x, y)} tests whether @tt{x} is less than or equal to @tt{y}.

@index{fxgt} @reachin{fxgt(x, y)} tests whether @tt{x} is greater than @tt{y}.

@index{fxge} @reachin{fxge(x, y)} tests whether @tt{x} is greater than or equal to @tt{y}.

@index{fxeq} @reachin{fxeq(x, y)} tests whether @tt{x} is equal to @tt{y}.

@index{fxne} @reachin{fxne(x, y)} tests whether @tt{x} is not equal to @tt{y}.

@subsubsection{Anybody}

@(mint-define! '("Anybody"))
@reach{
  Anybody.publish(); // race(...Participants).publish()
}

@index{Anybody} Reach provides a shorthand, @reachin{Anybody}, which serves as a
@reachin{race} between all @tech{participant}s.
This shorthand can be useful for situations where
it does not matter who @reachin{publish}es, such as in a @reachin{timeout}.

@reachin{Anybody} is strictly an abbreviation of a @reachin{race} involving all of the named participants of the application.
In an application with a @tech{participant class}, this means any principal at all, because there is no restriction on which principals (i.e. addresses) may serve as a member of that class.
In an application without any @tech{participant class}es, @reachin{Anybody} instead would mean only the actual previously-bound @tech{participant}s.

@subsubsection{Intervals}

An @reachin{Interval} is defined by

@(mint-define! '("Interval"))
@reach{
  export const Interval = Tuple(IntervalType, Int, Int, IntervalType); }

where @reachin{IntervalType} is defined by

@(mint-define! '("IntervalType"))
@reach{
  export const [ isIntervalType, Closed, Open ] = mkEnum(2);
  export const IntervalType = Refine(UInt, isIntervalType);  }

@subsubsub*section{Constructors}

An interval may be constructed with its tuple notation or by function:

@reach{
  // Representing [-10, +10)
  const i1 = [Closed, -10, +10, Open];
  const i2 = interval(Closed, -10, +10, Open);
  const i3 = intervalCO(-10, +10); }

For convenience, Reach provides a number of functions for constructing intervals:

@index{interval} @reachin{interval(IntervalType, Int, Int, IntervalType)} constructs an interval where the first and second argument
represent the left endpoint and whether it's open or closed; the third and fourth argument represent the right endpoint and whether it's open or closed.

@index{intervalCC} @reachin{intervalCC(l, r)} constructs a closed interval from two endpoints of type @reachin{Int}.

@index{intervalCO} @reachin{intervalCO(l, r)} constructs a half-open interval from two endpoints of type @reachin{Int} where the left endpoint is closed and the right endpoint is open.

@index{intervalOC} @reachin{intervalOC(l, r)} constructs a half-open interval from two endpoints of type @reachin{Int} where the left endpoint is open and the right endpoint is closed.

@index{intervalOO} @reachin{intervalOO(l, r)} constructs an open interval from two endpoints of type @reachin{Int}.

@subsubsub*section{Accessors}

@index{leftEndpoint} @reachin{leftEndpoint(i)} will return the @reachin{Int} that represents the left endpoint of an interval.

@index{rightEndpoint} @reachin{rightEndpoint(i)} will return the @reachin{Int} that represents the right endpoint of an interval.

@subsubsub*section{Relational Operations}

Intervals may be compared with the following functions:

@index{intervalEq} @reachin{intervalEq(l, r)} tests whether the intervals are equal.

@index{intervalNe} @reachin{intervalNe(l, r)} tests whether the intervals are not equal.

@index{intervalLt} @reachin{intervalLt(l, r)} tests whether the left interval is less than the right interval.

@index{intervalLte} @reachin{intervalLte(l, r)} tests whether the left interval is less than or equal to the right interval.

@index{intervalGt} @reachin{intervalGt(l, r)} tests whether the left interval is greater than the right interval.

@index{intervalGte} @reachin{intervalGte(l, r)} tests whether the left interval is greater than or equal to the right interval.

@subsubsub*section{Arithmetic Operations}

@index{intervalAdd} @reachin{intervalAdd(l, r)} adds the two intervals.

@index{intervalSub} @reachin{intervalSub(l, r)} subtracts the two intervals.

@index{intervalMul} @reachin{intervalMul(l, r)} multiplies the two intervals.

@index{intervalDiv} @reachin{intervalDiv(l, r)} divides the two intervals.

@subsubsub*section{Other Operations}

@(mint-define! '("intervalIntersection"))
@reach{
  const i1 = intervalOO(+3, +11); // (+3, +11)
  const i2 = intervalCC(+7, +9);  // [+7, +9]
  intervalIntersection(i1, i2);   // [+7, +11)  }

@index{intervalIntersection} @reachin{intervalIntersection(x, y)} returns the intersection of two intervals.

@(mint-define! '("intervalUnion"))
@reach{
  const i1 = intervalOO(+3, +9);  // (+3, +9)
  const i2 = intervalCC(+7, +11); // [+7, +11]
  intervalUnion(i1, i2);          // (+3, +11]  }

@index{intervalUnion} @reachin{intervalUnion(x, y)} returns the union of two intervals.

@(mint-define! '("intervalWidth"))
@reach{
  intervalWidth(intervalCC(+4, +45)); // +41 }

@index{intervalWidth} @reachin{intervalWidth(i)} returns the width of an interval.

@(mint-define! '("intervalAbs"))
@reach{
  intervalAbs(intervalCC(+1, +10)); // +10 }

@index{intervalAbs} @reachin{intervalAbs(i)} returns the absolute value of an interval.

