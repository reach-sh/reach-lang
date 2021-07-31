#lang scribble/manual
@(require scribble/bnf
          "lib.rkt")

@title[#:version reach-vers #:tag "ref-programs-step"]{Steps}

A Reach @tech{step} occurs in the @tech{continuation} of a @tech{deploy statement} or @tech{commit statement}.
It represents the actions taken by each of the participants in an application.

@section[#:tag "ref-programs-step-stmts"]{Statements}

Any statements valid for a @seclink["ref-programs-compute-stmts"]{computation} are valid for a step.
However, some additional statements are allowed.

@subsection[#:tag "ref-programs-only-step"]{@tt{only} and @tt{each}}

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

The @deftech{interact shorthand}, written @reachin{PART.interact.METHOD(EXPR_0, ..., EXPR_n)}, is available for calling an @reachin{interact} function
from outside of an @reachin{only} block. Such functions must return @reachin{Null}; therefore, they are only useful
if they produce side-effects, such as logging on the @tech{frontend}. For example, the
function @reachin{log} in the @tech{participant interact interface} of @reachin{Alice} may be called via:

@reach{
  Alice.interact.log(x); }

@(hrule)

@(mint-define! '("each"))
@reach{
 each([Alice, Bob], () => {
   const pretzel = interact.random(); }); }

An @deftech{each} @tech{local step} statement can be written as @reachin{each(PART_TUPLE () => BLOCK)}, where @reachin{PART_TUPLE} is a tuple of @tech{participants} and @reachin{BLOCK} is a @tech{block}.
It is an abbreviation of many @tech{local step} statements that could have been written with @reachin{only}.

@subsection{Pay Amounts}

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


@subsection{@tt{publish}, @tt{pay}, @tt{when}, and @tt{timeout}}

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

@(note-ctransfer)

A @tech{consensus transfer} is written @reachin{PART_EXPR.publish(ID_0, ..., ID_n).pay(PAY_EXPR)..when(WHEN_EXPR).timeout(DELAY_EXPR, () => TIMEOUT_BLOCK)},
where @reachin{PART_EXPR} is an expression that evaluates to a @tech{participant} or @tech{race expression},
@reachin{ID_0} through @reachin{ID_n} are identifiers for @reachin{PART}'s @tech{public} @tech{local state},
@reachin{PAY_EXPR} is a @tech{public} @tech{expression} evaluating to a @tech{pay amount},
@reachin{WHEN_EXPR} is a @tech{public} @tech{expression} evaluating to a boolean and determines if the @tech{consensus transfer} takes place,
@reachin{DELAY_EXPR} is a @tech{public} @tech{expression} that depends on only @tech{consensus state} and evaluates to a @tech{time argument},
@reachin{TIMEOUT_BLOCK} is a @tech{timeout} @tech{block}, which will be executed after the @reachin{DELAY_EXPR} @tech{time argument} passes without @reachin{PART} executing this @tech{consensus transfer}.

All of the expressions within a @tech{consensus transfer} are evaluated in a @deftech{pure} context, which may not alter the state of the
application.
The @reachin{PAY_EXPR}, @reachin{WHEN_EXPR}, and @reachin{DELAY_EXPR} expressions must refer only to the @tech{consensus state}, including the new data published via the @reachin{.publish} component.

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

A @tech{consensus transfer} binds the identifiers @reachin{ID_0} through @reachin{ID_n} for all @tech{participants} to the values included in the @tech{consensus transfer}, overwriting any bindings that already exist for those identifiers.
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
In the tail of this program, @reachin{x} is bound to either @reachin{1} or @reachin{2}, i.e., either @reachin{Alice} or @reachin{Bob}'s value is overwritten.
This overwriting applies even if @reachin{Alice} wins and @reachin{Alice} is a participant class, i.e., the value of @reachin{x} in the tail is guaranteed to be the single value that was agreed upon in the consensus.

@subsection{@tt{fork}}

@(mint-define! '("fork") '("paySpec"))
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

@(note-ctransfer)

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

The @reachin{TOKENS_EXPR} and @reachin{PAY_EXPR} have the same restrictions as the @reachin{.pay} component of a @tech{consensus transfer}: i.e., they must be @tech{pure} and can only refer to @tech{consensus state}.

The @reachin{.case} component may be repeated many times.

The same @tech{participant} may specify multiple cases. In this situation, the order of the cases is significant.
That is, a subsequent case will only be evaluated if the prior case's @tt{when} field is @reachin{false}.

If the @tech{participant} specified by @reachin{PART_EXPR} is not already @tech{fixed} (in the sense of @reachin{Participant.set}), then if it wins the @reachin{race}, it is @tech{fixed}, provided it is not a @tech{participant class}.

@subsubsection{@tt{fork} intuition}

A @tech{fork statement} is an abbreviation of a common @reachin{race} and @reachin{switch} pattern you could write yourself.

The idea is that each of the @tech{participants} in the @reachin{case} components do an independent @tech{local step} evaluation of a value they would like to @reachin{publish} and then all @reachin{race} to @reachin{publish} their value.
The one that "wins" the @reachin{race} then determines not only the value (& @reachin{pay} expression), but also what @tech{consensus step} code runs to consume the value.

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

@subsection{@tt{wait}}

@(mint-define! '("wait"))
@reach{
 wait(TIME); }

A @deftech{wait statement}, written @reachin{wait(TIME);}, delays the computation until the @reachin{TIME} @tech{time argument} passes.
@reachin{TIME} must be @tech{pure} and only reference values known by the @tech{consensus state}.
It may only occur in a @tech{step}.

@subsection{@tt{exit}}

@(mint-define! '("exit"))
@reach{
 exit(); }

An @deftech{exit statement}, written @reachin{exit();}, halts the computation.
It is a @tech{terminator statement}, so it must have an empty @tech{tail}.
It may only occur in a @tech{step}.

@section[#:tag "ref-programs-step-exprs"]{Expressions}

Any expressions valid for a @seclink["ref-programs-compute-exprs"]{computation} are valid for a step.
However, some additional expressions are allowed.

@subsection{@tt{race}}

@(mint-define! '("race"))
@reach{
 race(Alice, Bob).publish(bet); }

@(note-ctransfer)

A @deftech{race expression}, written @reachin{race(PARTICIPANT_0, ..., PARTICIPANT_n);}, constructs a @tech{participant} that may be used in a @tech{consensus transfer} statement, such as @reachin{publish} or @reachin{pay}, where the various @tech{participants} race to be the first one to perform the @tech{consensus transfer}.

Reach provides a shorthand, @reachin{Anybody}, which serves as a @reachin{race} between all the @tech{participants}.

@margin-note{See @seclink["guide-race"]{the guide section on races} to understand the benefits and dangers of using @reachin{race}.}

@subsection{@tt{unknowable}}

@(mint-define! '("unknowable"))
@reach{
 unknowable( Notter, Knower(var_0, ..., var_N), [msg] ) }

@index{unknowable} A @tech{knowledge assertion} that the @tech{participant} @reachin{Notter} @emph{does not} know the results of the variables @reachin{var_0} through @reachin{var_N}, but that the @tech{participant} @reachin{Knower} @emph{does} know those values.
It accepts an optional bytes argument, which is included in any reported violation.

@subsection{@tt{closeTo}}

@(mint-define! '("closeTo"))
@reach{
 closeTo( Who, after, nonNetPayAmt ) }

@index{closeTo} Has @tech{participant} @reachin{Who} make a @tech{publication}, then @tech{transfer} the @reachin{balance()} and the non-network @tech{pay amount} to @reachin{Who} and end the @|DApp| after executing the function @reachin{after} in a @tech{step}.
The @reachin{nonNetPayAmt} parameter should be a @tech{pay amount}. For example, when closing a program that uses a @reachin{Token} @reachin{token}, the argument would be @reachin{[ [balance(tok), tok] ]}.
The @reachin{after} and @reachin{nonNetPayAmt} argument are optional.


