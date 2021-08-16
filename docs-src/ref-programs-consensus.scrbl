#lang scribble/manual
@(require scribble/bnf
          "lib.rkt")

@title[#:version reach-vers #:tag "ref-programs-consensus"]{Consensus Steps}

A Reach @tech{consensus step} occurs in the @tech{continuation} of a @tech{consensus transfer} statement.
It represents the actions taken by the @tech{consensus network} @tech{contract} of an application.

@section[#:tag "ref-programs-consensus-stmts"]{Statements}

Any statements valid for a @seclink["ref-programs-compute-stmts"]{computation} are valid for a consensus step.
However, some additional statements are allowed.

@subsection{@tt{commit}}

@(mint-define! '("commit"))
@reach{
 commit(); }

A @deftech{commit statement}, written @reachin{commit();}, @tech{commits} to @tech{statement}'s @tech{continuation} as the next @tech{step} of the @DApp computation. In other words, it ends the current @tech{consensus step} and allows more @tech{local steps}.

@subsection[#:tag "ref-programs-only-consensus"]{@tt{only} and @tt{each}}

@secref["ref-programs-only-step"] are allowed in @tech{consensus steps} and are executed by @tech{backends} once they observe the completion of the @tech{consensus step} (i.e., after the associated @tech{commit statement}.)

@subsection[#:tag "ref-programs-consensus-view"]{View Objects}
@(note-view-xref)

@reach{
  vNFT.owner.set(creator);
}

If @reachin{VIEW} is a @deftech{view object}, then its fields are the elements of the associated @tech{view}.
Each of these fields are bound to an object with an @litchar{set} method that accepts the function or value to be bound to that @tech{view} at the current step, and all steps @tech{dominated} by the current step (unless otherwise overridden.)
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

@subsection{@tt{Participant.set} and @tt{.set}}

@(mint-define! '("Participant.set"))
@reach{
 Participant.set(PART, ADDR);
 PART.set(ADDR); }

@index{Participant.set} After execution, the given @tech{participant} is @tech{fixed} to the given address.
It is @tech{invalid} to attempt to @reachin{.set} a @tech{participant class}.
If a @tech{backend} is running for this @tech{participant} and its address does not match the given address, then it will abort.
This may only occur within a @tech{consensus step}.

@margin-note{@secref["workshop-relay"] is a good introductory project that demonstrates how to use this feature of Reach.}

@subsection{@tt{while}}

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
 DEFINE_BLOCK; // optional
 invariant(INVARIANT_EXPR);
 while( COND_EXPR ) BLOCK }

where @reachin{LHS} is a valid left-hand side of an @tech{identifier definition} where the @tech{expression} @reachin{INIT_EXPR} is the right-hand side, and
@reachin{DEFINE_BLOCK} is an optional @tech{block} that may define bindings that use the @reachin{LHS} values which are bound inside the rest of the @reachin{while} and its @tech{tail}, and
@reachin{INVARIANT_EXPR} is an @tech{expression}, called the @deftech{loop invariant}, that must be true before and after every execution of the @tech{block} @reachin{BLOCK}, and
if @reachin{COND_EXPR} is true, then the @tech{block} executes,
and if not, then the loop terminates and control transfers to the @tech{continuation} of the @tech{while statement}.
The identifiers bound by @reachin{LHS} are bound within @reachin{DEFINE_BLOCK}, @reachin{INVARIANT_EXPR}, @reachin{COND_EXPR}, @reachin{BLOCK}, and the @tech{tail} of the @tech{while statement}.

@margin-note{Read about finding @seclink["guide-loop-invs"]{loop invariants} in the Reach guide.}

@subsection{@tt{continue}}

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

A @tech{continue statement} must be @tech{dominated} by a @tech{consensus transfer}, which means that the body of a @tech{while statement} must always @reachin{commit();} before calling @reachin{continue;}. A continue statement may occur in a step, provided the @reachin{RHS} modifies the state into a consensus step.
This restriction may be lifted in future versions of Reach, which will perform termination checking.

@subsection{@tt{parallelReduce}}

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

@(note-ctransfer)

A @deftech{parallel reduce statement} is written:

@(mint-define! '("paySpec") '("define"))
@reach{
const LHS =
  parallelReduce(INIT_EXPR)
  .define(() => DEFINE_BLOCK)
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
the @reachin{DEFINE_BLOCK} is like the @reachin{DEFINE_BLOCK} of a @reachin{while} loop;
while the @reachin{.case}, @reachin{.timeout}, and @reachin{.paySpec} components are like the corresponding components of a @reachin{fork} statement.

The @reachin{.case} component may be repeated many times, provided the @reachin{PART_EXPR}s each evaluate to a unique @tech{participant}, just like in a @reachin{fork} statement.

The @reachin{.define} component may define bindings that reference the @reachin{LHS} values. These bindings are accessible
from every component of the @reachin{parallelReduce} statement, except for the @reachin{INIT_EXPR}.

@subsubsection{@tt{.timeRemaining}}

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

@subsubsection{@tt{.throwTimeout}}

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


@subsubsection{@tt{parallelReduce} intuition}

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

@section[#:tag "ref-programs-consensus-exprs"]{Expressions}

Any expressions valid for a @seclink["ref-programs-compute-exprs"]{computation} are valid for a consensus step.
However, some additional expressions are allowed.

@subsection[#:tag "ref-programs-consensus-this"]{@tt{this}}

Inside of a @tech{consensus step}, @reachin{this} refers to the address of the participant that performed the @tech{consensus transfer}.
This is useful when the @tech{consensus transfer} was initiated by a @reachin{race} expression.

@subsection{@tt{transfer}}

@(mint-define! '("transfer"))
@reach{
 transfer(10).to(Alice);
 transfer(2, gil).to(Alice); }

A @deftech{transfer expression},
written @reachin{transfer(AMOUNT_EXPR).to(ADDR_EXPR)},
where @reachin{AMOUNT_EXPR} is an @tech{expression} that evaluates to an unsigned integer, and
@reachin{ADDR_EXPR} evaluates to an address,
performs a @tech{transfer} of @tech{network tokens} from the @tech{contract} to the named @tech{participant}.
@reachin{AMOUNT_EXPR} must evaluate to less than or equal to the balance of @tech{network tokens} in the @tech{contract} @tech{account}.

A @tech{transfer expression} may also be written @reachin{transfer(AMOUNT_EXPR, TOKEN_EXPR).to(ADDR_EXPR)},
where @reachin{TOKEN_EXPR} is a @reachin{Token},
which @tech{transfers} @tech{non-network tokens} of the specified type.

A @tech{transfer expression} may only occur within a @tech{consensus step}.

@subsection{@tt{require}}

@(mint-define! '("require"))
@reach{
 require( claim, [msg] ) }

@index{require} A @tech{requirement} where @reachin{claim} evaluates to @reachin{true} with @tech{honest} @tech{participants}.
This may only appear in a @tech{consensus step}.
It accepts an optional bytes argument, which is included in any reported violation.

@subsection{@tt{checkCommitment}}

@(mint-define! '("checkCommitment"))
@reach{
 checkCommitment( commitment, salt, x ) }

@index{checkCommitment} Makes a @tech{requirement} that @reachin{commitment} is the @tech{digest} of @reachin{salt} and @reachin{x}.
This is used in a @tech{consensus step} after @reachin{makeCommitment} was used in a @tech{local step}.

@subsection{Token minting}

@(mint-define! '("burn") '("destroy") '("supply") '("destroyed"))
@reach{
  require(supply >= 2 * amt);
  const tok = new Token({name, symbol, url, metadata, supply});
  transfer(amt, tok).to(who);
  tok.burn(amt);
  assert(tok.supply() == supply - amt);
  tok.burn();
  assert(tok.destroyed() == false);
  tok.destroy();
}

@margin-note{
  @secref["ref-networks"] discusses how Reach supports @tech{token minting} on specific consensus networks.
}

A @tech{non-network token} may be @deftech[#:key "token minting"]{minted} with the expression @reachin{new Token(PARAMS)}, where @reachin{PARAMS} is an object with the following keys:
@itemlist[
@item{@litchar{name}: A value of type @reachin{Bytes(32)}; defaults to empty.}
@item{@litchar{symbol}: A value of type @reachin{Bytes(8)}; defaults to empty.}
@item{@litchar{url}: A value of type @reachin{Bytes(96)}; defaults to empty.}
@item{@litchar{metadata}: A value of type @reachin{Bytes(32)}; defaults to empty.
This value is intended to be a @tech{digest} of a larger metadata document.}
@item{@litchar{supply}: A value of type @reachin{UInt}; defaults to @reachin{UInt.max}.}
]

This returns a @reachin{Token} value and deposits a @reachin{supply} amount of the new @tech{non-network tokens} into the @tech{contract} account associated with the @|DApp|.
These tokens must be destroyed by the end of the @|DApp|.

@(hrule)

@reachin{Token.burn(tok, amt)}, or @reachin{tok.burn(amt)}, where @reachin{tok} is a @reachin{Token} value and @reachin{amt} is a @reachin{UInt} value, may be used to @deftech{burn} tokens in the @tech{contract} account, meaning that they are utterly destroyed and can never be recovered.

@(hrule)

@reachin{Token.destroy(tok)}, or @reachin{tok.destroy()}, where @reachin{tok} is a @reachin{Token} value, may be used to destroy the token so that it may never be used again by any users on the @tech{consensus network}.
This must be called before the application exits.

@(hrule)

@reachin{Token.destroyed(tok)}, or @reachin{tok.destroyed()}, where @reachin{tok} is a @reachin{Token} value, returns whether @reachin{destroy}
has been called on @reachin{tok} yet.

@(hrule)

@reachin{Token.supply(tok)}, or @reachin{tok.supply()}, where @reachin{tok} is a @reachin{Token} value, may be used to query the current supply of tokens, i.e. the number of tokens which have not been @tech{burn}t.

@subsection{Remote objects}

@(mint-define! '("remote"))
@reach{
  const randomOracle =
    remote( randomOracleAddr, {
      getRandom: Fun([], UInt),
    });
  const randomVal = randomOracle.getRandom.pay(randomFee)();
}

@margin-note{
  @secref["ref-networks"] discusses how Reach supports @tech{remote objects} on specific consensus networks.
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

@subsection{Mappings: creation and modification}

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

@subsection{Sets: creation and modification}

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
