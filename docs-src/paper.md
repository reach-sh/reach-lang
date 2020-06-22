# Reach: DApps as Easy as Apps: Blockchain for Everyone

Jay McCarthy <jay@reach.sh>

CTO, Reach

Associate Professor, University of Massachusetts Lowell

# Introduction

Reach is a domain-specific language for writing decentralized
applications. It provides automatic solutions to a few key problems
faced by blockchain developers: verifying that the DApp is
trustworthy, ensuring the smart contract is consistent with
client-side software, and abstracting over different blockchains.

Reach programs embed descriptions of properties about the behavior of
the application: safety properties assert mistakes do not occur, and
liveness properties assert desired outcomes do occur. The compiler
automatically adds soundness properties that assert the application
does not violate fundamental expectations of all DApps, such as that
they conserve resources. The compiler automatically verifies
correctness of these properties using the Z3 SMT theorem prover
without intervention from programmers. This ensures that Reach
programs are not susceptible to attacks that steal their resources,
and ensures that untrusting participants can rely on the integrity and
validity of the Reach program.

Reach programs incorporate the client-side behavior of participants,
as well as on-chain behavior of the contract. The Reach compiler uses
End-Point Projection to extract programs for each party and the
contract, while guaranteeing each side makes the same assumptions
about application state and communication protocols. This ensures that
attacks do not exist that exploit the slightly different semantics of
blockchain virtual machines and client-side programming languages.

Reach uses a blockchain-agnostic model of computation that allows
programs to target different chains, including scaling solutions. This
ensures that DApps can be designed independently of the deployment
details and not be tied to the particular vagaries of any one
platform.

The core philosophy of Reach is to design a highly constrained
programming language that makes it easy to automatically prove the
structural components of desirable properties about DApps, and makes
it possible to easily prove the user-specific components of those
properties. This is in contrast to designing an unconstrained language
and providing a novel proving technique specialized for decentralized
applications.

In this article, we take walk-through of an example Reach program and
show how Reach performs each function.

# External References and Example Program

The Reach repository is located at:

https://github.com/Reach-sh/reach-lang

This article is available at:

https://github.com/Reach-sh/reach-lang/blob/master/docs/paper.md

In this article, we will repeatedly refer to a simple example program:

https://github.com/Reach-sh/reach-lang/blob/master/examples/rps/rsh/rps.rsh

And the results of Reach's compilation of the program:

https://github.com/Reach-sh/reach-lang/tree/master/examples/rps/build

This program models a wager on the result of a game of
Rock-Paper-Scissors between Alice and Bob. In the game, Alice
transfers a wager and an escrow into the contract along with a
commitment of her hand, via hashing it with a random salt. Next, Bob
transfers the wager and reveals his hand. Then, Alice reveals her hand
by publishing the salt and the hand. At this point, the outcome of the
game is determined and the winner receives both wagers, while Alice
receives the escrow back.

# Computation Model & Program Walkthrough

Reach programs define the interactions between a set of
participants as they reach consensus on the results of computations
based on variables initially known only to one participant. For now,
the number of interactions and participants are fixed, finite, and
known at the beginning of the program. (We discuss lifting these
limitations in [Future Work](#future-work).)

In our example program, the participants are `A` ("Alice") and `B`
("Bob"). Alice knows the amount of the wager, the amount of escrow it
will deposit in the contract, and her hand. Bob knows his hand. We
express this in the language by writing:

```
participant A {
    uint256 wagerAmount,
    uint256 escrowAmount,
    uint256 handA }

participant B {
    uint256 handB }
```

In Reach, each participant performs a block of local actions until
they reach a point at which a consensus is necessary. After resolving
the consensus block, they return to local actions. Local actions are
assumed to be run by all participants, but some actions may be
annotated with the single party that takes them.

In our example program, in the first local block each participant
makes claims about their initial knowledge via the `assume!` form,
which is a kind of assertion. (In [Verification](#verification), we
discuss the subtleties of the various kinds of assertion in Reach.)
These two claims merely state that the hand values, which are
arbitrary unsigned integers, are within the fixed range of the
enumeration of hand values.

```
main {
    @A assume! isHand(handA);
    @B assume! isHand(handB);
```

Next, Alice computes the commitment by calling a library function
`precommit` and receiving the multiple values it returns. It binds
these values to the new constants `commitA` (the commitment) and
`saltA` (the random salt.)

```
    @A const commitA, saltA = precommit(handA);
```

At this point, the first consensual action will occur. Alice needs to
publish the terms of the bet (`wagerAmount` and `escrowAmount`) and
her commitment (`commitA`). She also needs to actually transfer this
amount of resources into the contract's account. Finally, all parties
need to agree that Alice actually sent this information.

There is a small wrinkle, however. Reach uses an information-flow
security type system to ensure that participants do not accidentally
reveal secret information. All information that only one participant
knows is assumed to be secret until explicitly declassified. In this
case, Alice's knowledge of the terms of the bet and her commitment are
secret. Therefore, Alice needs to first declassify this information
before performing the transfer.

```
    @A declassify! wagerAmount;
    @A declassify! escrowAmount;
    @A declassify! commitA;
    @A publish! wagerAmount, escrowAmount, commitA
       w/ (wagerAmount + escrowAmount);
    commit;
```

The first two lines perform the declassification, while the next two
perform the publishing and payment to the contract. The last line
(`commit;`) finishes the consensual block and returns to the next
local block. After this statement, it is now consensual knowledge that
Alice shared these three values and transferred the appropriate
amount.

There is no next local block, however, because there is no additional
computation necessary. Instead, we move immediately to the next
consensus block, which is initiated by Bob. In Reach, there is
always exactly one participant that initiates a consensus block. (This
is not intrinsic to decentralized applications, but a particular
limitation of the first version of Reach. In [Future
Work](#future-work), we discuss lifting this limitation.)  In this
block, Bob declassifies his hand, publishes it, and transfers the
wager amount, which he has just learned from the last consensus block.

```
    @B declassify! handB;
    @B publish! handB w/ wagerAmount;
    require! isHand(handB);
    commit;
```

This consensus block, however, does not immediately return to local
control. In addition to verifying that Bob actually transferred the
wager amount, all parties also claim that Bob's hand is valid. In this
case, we use a new kind of claim (`require!`) rather than the one used
initially by Bob (`assume!`). Although Bob has already checked that
the hand is valid, that claim was not consensual, so Alice cannot rely
on it. Thus, Alice needs to verify the claim as soon as she learns the
value. If Bob were dishonest and did not actually check the claim in
the program, it would be consensually verified at this point and Bob's
attempt to publish it would be rejected.

The last consensus block is where a lot of action is going to
happen. We will break it down into a number of steps.

First, Alice publishes the inputs to the commitment, after
declassifying them, and we consensually verify that the earlier
commitment actually is made from these inputs:

```
    @A declassify! saltA;
    @A declassify! handA;
    @A publish! saltA, handA w/ 0;
    check_commit(commitA, saltA, handA);
```

This block indicates that the consensus retains knowledge of the prior
commitment by Alice, because the `commitA` variable is still in scope
in the consensus. Once the consensus knows Alice's hand, and that it
is the same as was committed to earlier, we can check that it is valid
and determine the winner.

```
    require! isHand(handA);
    const outcome = winner(handA, handB);
    [....]
```

Once the winner is known, we can compute the winnings and transfer
them to the appropriate parties:

```
    const getsA, getsB =
          if (outcome == A_WINS) {
              values (2 * wagerAmount), 0 }
          else if (outcome == B_WINS) {
              values 0, (2 * wagerAmount) }
          else {
              values wagerAmount, wagerAmount };
    transfer! A <- (escrowAmount + getsA);
    transfer! B <- getsB;
    commit;
```

The computation is now over and the two parties simply return the
final outcome:

```
    [....]
    outcome }
```

This program never explicitly deals with the low-level issues of
writing blockchain programs: *there are no block numbers, gas
calculations, calling contract methods, subscribing to contract
events, and so on*. From Reach's perspective, a blockchain is
simply mutual knowledge about a monotonically increasing list of
values, where the validity of each block of values depends on the
previous values. We could express this as the following type:

```
Block := List Value
Chain := List Block

Contract := {
    append : Chain x Block -> Maybe Chain
}
```

An `append` operation only succeeds (returns `Just next_chain`) if the
values are consistent with the constraints imposed by the consensus
block. The prior chain is an argument to the `append` function because
the prior blocks may influence the constraints on the current
block.

In practice, we assume a slightly simpler model of a blockchain
contract by abstracting chains into a state type specific to the
particular contract.

```
Contract State := {
    initial : State
    observe : State x Block -> Maybe State
}
```

Although it is possible for this state to be the entire chain, it is
often more efficient to select a smaller type. From a particular
chain, it is always possible to discover the current state by folding
the `observe` function over the blocks on the chain.

Similarly, participants in a decentralized application are modeled as
agents with private knowledge that maybe react to blocks they observe
on the chain.

```
Participant Internal := {
    start : Internal x Maybe Block
    react : Internal x State x Block
         -> Internal x Maybe Block
```

The `start` object represents the initial private knowledge of the
participant and whether they are the first publisher. The `react`
function updates their internal state based on the current state of
the contract and the most recent message, as well as potentially
publishes another message. Again, given a chain and a participant, we
can always determine how that participant would react to each action.

This computation model (`Contract` and `Participant`) defines the
expectations that Reach has on blockchains and client platforms
that it deploys to. In practice, the complexity of the `observe`
function determines whether a particular Reach program could deploy
to a particular chain. We have purposefully designed the computational
abilities of Reach to map to the lowest-common-denominator chains.

# Analysis

The Reach compiler performs some analyses on the source program to
verify a number of essential safety properties. Each of these analyses
can be expressed as a constraint on Reach programs.

Reach is type-safe, so all operations must receive the correct
number and type of arguments. The type system is simple, however, so
it is always possible to determine the types of intermediate
expressions based on the types of the arguments. The only annotations
programmers are required to add are on the initial knowledge of the
participants. This ensures that there are no unsafe operations
performed on illegal input, which could lead to errors like buffer
overflows or type confusion.

Reach mandates that all computations are finite. This is enforced by
not including looping forms like `while` and `for` and disallowing
recursive functions in the computation fragment. This ensures that all
computations can run within estimable bounds.

However, Reach allows interactions to take arbitrarily many steps
through a `do until` form that repeated executes a series of
interactions until a formula is satisfied.

Reach's model assumes that all interactions have a consensual next
initiator. This means that local actions can only determine the values
in the interactions, not the structure of the interaction. In
practice, this means that `if` statements must either only produce
values, or they must be located inside of consensus blocks. This is an
essential property to avoid confusion where participants have disunity
on the state of the program.

Similarly, there are certain primitives that cannot be called from the
consensus, but are allowed to be called from the participants. These
are `random`, for generating a random number, and `interact` for
interacting with the frontend that drives the Reach program. (We
discuss `interact` more in [End-Point
Projection](#end-point-projection).)

Actors in Reach, i.e. the participants and the contract, always
require a single next action that will be run. In programming language
theory parlance, this means that the continuation of every statement
must be statically knowable. This is enforced by an A-Normal
Form-style transformation that exposes the continuation of every
expression. The most subtle aspect of this is that the continuations
of impure `if` statements must be inlined into the two branches,
duplicating code, to enforce the previously mentioned conditions on
`if`s. The compiler does extra purity analysis to turn `if`s in the
source code into conditional moves in the intermediate representations
to avoid code growth.

Reach's type system is information-flow sensitive. As mentioned
before, all initial knowledge of participants is marked as secret by
Reach. Any transformation that involves secret information in any way
produces secret information. Most importantly, this means that `if
Secret then Public else Public` produces secret information, not
public, because a secret value was used to compute the branch
taken. When Reach programmers attempt to publish information, the
compiler refuses to continue if the information is secret; instead,
the programmer must explicitly declassify it before sending. We could
remove these annotations by always assuming that published information
is implicitly declassified, but we view manual declassification
annotations as a fundamental step in security auditing: programmers
should have to explicitly decide when something is free to release to
the public.

Reach's variable scope rules are subtle because programs involve the
actions of many parties. During type checking, the compiler must
ensure that each participant is only relying on values that they
possess, whether because they initially knew them, or gained them via
publications by other participants.

Each of these analyses work together to form a basic kind of soundness
for Reach programs that the rest of the compiler suite rely on.

# Verification

Reach programs embed statements of logical properties of their
correctness. In addition to these program-specific properties, Reach
automatically embeds claims that resources are preserved and the
contract's balance is zero at the end of the program.

The Reach compiler proves these properties by representing the program
as an SMT problem and delivers it to an SMT solver (e.g. Z3) for
verification.

(Skip this paragraph if you do not need an introduction to SMT.)
Satisfiability Modulo Theories (SMT) is a decision problem on logical
formulas and sets of equational theories. SMT can be seen as an
optimization of satisfiability (SAT). A SAT problem concerns a set of
boolean variables ($x_0$, $x_1$, ... $x_n$) and a formula over them
(e.g., $x_0 \vee \neg x_1 \implies x_2$). The SAT solver determines if
there is an assignment of the variables to values such that the
formula evaluates to true. SAT was the first problem proved to be
NP-complete. If NP does not equal P, then SAT is intractable and there
is no solution that is not exponential. SMT generalizes SAT by adding
"sorts" (which are types to normal programmers), functions that
operate on these new sorts, and equations that relate different
functions together. For example, in SMT, `boolean` is a sort, `not` is
a function from boolean to boolean and `not (not x) = x` is an
equation. Rich SMT solvers have many more theories, such as a theory
of natural number, bit vectors, arrays, and so on. They also give
users the ability to define new sorts and new equational theories over
them. The main thing that a SMT solver does is determine if a formula
is satisfiable, i.e. there exists an assignment of variables to values
where the formula is true. Most SMT solvers, including Z3 which we
use, also provide the ability to derive models, which are the actual
values that satisfy the formula. (It is important to understand that
simply determining if an assignment exists does not entail that you
know the values.)

Given the low-computational complexity of Reach and the A-Normal Form
of the intermediate language, representing Reach programs as SMT
problems is simple: each variable definition in the intermediate
language becomes a variable in the SMT problems of the appropriate
sort and is constrained to be equal to the right-hand side of the
variable definition. We extend the set of sorts and theories to deal
with the particular kinds of values, like message digests and byte
strings, used in Reach programs. (See the
[build/rps.z3](https://github.com/Reach-sh/reach-lang/blob/master/examples/rps/build/rps.z3)
file for an example Z3 verification session.)

Reach verifies the correctness of each property from the perspective
of each participant, as well as the contract, and under a "trusted"
and an "untrusted" mode. In the "trusted" perspective, participants'
SMT problem includes the actions for all participants.  While in the
"untrusted" perspective, these actions are ignored and only the
particular participant's actions are included. These different modes
correspond to trust because if Alice's SMT problem includes Bob's
actions, then Alice is trusting that Bob will actually perform
them. In contrast, if Alice's problem does not include them, then from
the perspective of the SMT problem, the values Bob publishes are
completely unconstrained.

Reach supports four kinds of properties:

**Assumptions** (`assume!`) are checked at runtime, and if false, the
program aborts, and assumed to be true when included in the SMT
problem. This means that they become SMT assertions. In our example
program, Alice and Bob both assume that their hands are valid. These
statements cannot be verified by Z3, because they are based on values
from outside of the Reach program.

**Assertions** (`assert!`) are ignored at runtime and are verified by
the SMT problem. They are verified by asserting their negation and
checking for satisfiability. If the assertion is true, there should be
no assignment of the variables in the program that make the statement
false, so the SMT solver should return an `UNSAT` result. In our
example program, we include two assertions:

```
    assert! ((outcome == A_WINS) => isHand(handA));
    assert! ((outcome == B_WINS) => isHand(handB));
```

These establish that if Alice or Bob submit an invalid hand, then they
are not the winner. This property would be false if we incorrectly
implemented the outcome calculation and did not check for validity of
hands inside it. This is a logical property of the game itself and
helps to establish trust in the Reach program on the part of
users. Additionally, Reach automatically generates an assertion that
the balance of the contract is zero at the end of the program
run. This ensures that no resources are lost by the contract.

Assertions are used by Reach programmers to check that
program-specific safety properties are respected by the program.

**Requirements** (`require!`) are checked at runtime, and if false,
the program aborts, and behave differently in the SMT problem
depending on the mode. In trusted mode, they behave as assertions and
are verified. While in untrusted mode, they are assumed to be
true. This may sound counter-intuitive because lack of trust seems to
suggest that they are suspect and should be checked. However,
untrusted mode really refers to not assuming that the other
participants followed the program. This means that we can't rely on
any particular value being produced and need to make some sort of
assumption on what it is. Hence, the requirement states the
assumptions that are necessary for the continuation to be correct. In
contrast, trusted mode actually proves that these assumptions are met
by the actual participants, which is why trusted requirements are
treated with suspicion and verified. In our example, we use
requirements to verify that Alice and Bob's hands are valid and that
Alice actually submits the random salt and hand that she previously
committed to. In addition to these requirements specified by the
programmer, Reach automatically generates requirements that the amount
transferred to the consensus at each interaction is the same as is
specified in the program (e.g., Bob actually transmits the wager
amount.)

**Possibilities** (`possible?`) are ignored at runtime and are checked
for satisfiability in the SMT problem. Unlike assertions, these are
not negated in the SMT problem. This means that we are verifying that
it is possible for some values of inputs to arrive at the truth of the
statement. In our example program, we explore six possibilities,
abstracted with a function in the real code:

```
    possible? ((handA == ROCK) && (outcome == A_WINS));
    possible? ((handA == PAPER) && (outcome == A_WINS));
    possible? ((handA == SCISSORS) && (outcome == A_WINS));
    possible? ((handB == ROCK) && (outcome == B_WINS));
    possible? ((handB == PAPER) && (outcome == B_WINS));
    possible? ((handB == SCISSORS) && (outcome == B_WINS));
```

These establish that the game is fair and it is possible for both
Alice and Bob to be the winner. This would be false if we incorrectly
implemented the outcome calculation such that one party always won, or
if there was a flaw in the communication such that Bob could observe
Alice's hand and always win or, if Alice could observe Bob's hand and
change her commitment.

Possibilities are used by Reach programmers to check that
program-specific liveness properties are respected by the program,
thereby increasing trust in the game.

---

The verification offered by Reach lowers the degree of trust that
users need to place in decentralized applications. Rather than
auditing their entire source code, they must only inspect the
assertions and possibilities. In our example application, Reach proves
95 different theorems about the program in about 1,300 steps of Z3
interaction. This verification must be done manually on decentralized
applications implemented without Reach.

# End-Point Projection

Reach programs describe the entire behavior of the decentralized
application: the behavior of each individual client-side participant,
as well as the on-chain behavior of the contract. Unlike traditional
languages, where compilation results in a single binary, Reach
compilations result in N clients, one for each participant, and a
contract. In our example program, there is a client for Alice and Bob,
plus a contract. The clients are contained in one source file,
[build/rps.mjs](https://github.com/Reach-sh/reach-lang/blob/master/examples/rps/build/rps.mjs),
while the contract is in
[build/rps.sol](https://github.com/Reach-sh/reach-lang/blob/master/examples/rps/build/rps.sol).

This process of compiling multiple end-points from a single source
program is called end-point projection and is one stage of our
compiler. (The result of this stage is its own program,
[build/rps.bl](https://github.com/Reach-sh/reach-lang/blob/master/examples/rps/build/rps.bl).)
In this discussion, we will mix the process of projection with the
process of compiling to the target languages.

**Contracts.** We'll start with the process of projecting the
contract.

The contract will have a single state variable. Throughout the run of
the program, this will be a digest of the step of the computation and
the free variables in the computation's continuation. Initially, the
step is `0` and the free variables are the identities of the
participants. This ensures that the size of the contract state is
minimal, at the expense of increasing the size of transactions. In the
future, we intend to do linear optimization to determine which is more
efficient.

```
contract ReachContract is Stdlib {
  uint256 current_state;

  constructor(address payable pA, address payable pB) public payable {
    current_state = uint256(keccak256(abi.encodePacked(uint256(0), pA, pB))); }
```

Each consensus block in the program will become a method in the
contract. Its arguments will be (a) the free variables in the
continuation and (b) the publication of the initiator; technically,
these are free variables as well. We will then ensure that the current
state is the same as the free variables, plus the step number, that
the sender of the message is the correct initiator, and that the value
of the message is as expected. Once these requirements have been
validated, we emit an event to the blockchain confirming that the
publication occurred and is valid. Finally, we update the state with
the next step number and the free variables of that step.

Here is the first step of the example program:

```
  event e0(uint256 v15, uint256 v16, uint256 v14);
  function m0(address payable pA, address payable pB, uint256 v15, uint256 v16, uint256 v14) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), pA, pB))));
    require(msg.sender == pA);
    require((msg.value == (v15 + v16)));
    emit e0(v15, v16, v14);
    current_state = uint256(keccak256(abi.encodePacked(uint256(1), pA, pB, v14, v15, v16))); }
```

In this step, the free variables are `pA` and `pB` (the identities of
the participants) and the message are the three variables
`v15/wagerAmount`, `v16/escrowAmount`, and `v14/commitA`. We require
that the state is one where the step is `0` and the participants are
the same as from the constructor. We require that `pA` is the sender
and that they transmitted the correct deposit. We then publish `e0`
with this information and update the state.

The second step is similar, but contains an extra requirement, when
the contract checks that Bob's hand is valid:

```
  event e1(uint256 v21);
  function m1(address payable pA, address payable pB, uint256 v14, uint256 v15, uint256 v16, uint256 v21) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), pA, pB, v14, v15, v16))));
    require(msg.sender == pB);
    require((msg.value == v15));
    // Check that Bob's hand is valid
    require(((uint256(0) <= v21) ? (v21 < uint256(3)) : false));
    emit e1(v21);
    current_state = uint256(keccak256(abi.encodePacked(uint256(2), pA, pB, v14, v15, v16, v21))); }
```

In the second step, we can observe that the event only includes the
information in the call and leaves out everything that can be computed
by the observers, such as new information computed by the contract and
values published in earlier steps. In the future, we will optimize
this by publishing an event with no information and compile our
clients so they inspect the transaction that generated the event for
the values in the arguments.

The final step will do more work, as it needs to transfer funds from
the contract to Alice and Bob.

```
 event e2(uint256 v27, uint256 v28);
  function m2(address payable pA, address payable pB, uint256 v14, uint256 v15, uint256 v16, uint256 v21, uint256 v27, uint256 v28) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(2), pA, pB, v14, v15, v16, v21))));
    require(msg.sender == pA);
    require((msg.value == uint256(0)));
    // Note 1
    require((v14 == (uint256(keccak256(abi.encodePacked((BCAT((abi.encodePacked(v27)), (abi.encodePacked(v28))))))))));
    require(((uint256(0) <= v28) ? (v28 < uint256(3)) : false));
    bool v41 = (uint256(0) <= v28) ? (v28 < uint256(3)) : false;
    bool v44 = (uint256(0) <= v21) ? (v21 < uint256(3)) : false;
    // Note 2
    uint256 v51 = (v41 ? v44 : false) ? ((v28 + (uint256(4) - v21)) % uint256(3)) : (v41 ? uint256(2) : (v44 ? uint256(0) : uint256(1)));
    bool v67 = v51 == uint256(2);
    bool v69 = v51 == uint256(0);
    // Note 3
    pA.transfer((v16 + (v67 ? (uint256(2) * v15) : (v69 ? uint256(0) : v15))));
    pB.transfer((v67 ? uint256(0) : (v69 ? (uint256(2) * v15) : v15)));
    emit e2(v27, v28);
    // Note 4
    current_state = 0x0;
    selfdestruct(address(0x02B463784Bc1a49f1647B47a19452aC420DFC65A)); } }
```

At note 1, we check that Alice's salt and hand hash to the same value
that was previously published. At note 2, we compute the outcome. At
note 3, we perform the transfer. Finally, at note 4, we receive the
refund on modifying the current state, and thereby ensure that no
other calls can be made, then self-destruct. We previously proved that
the account balance is zero, so there are no resources actually
transferred.

**Participants.** Projecting clients is a relatively straight-forward
process. Local blocks have already been transformed into A-Normal
Form, so there is a trivial list of variable definitions with simple
right-hand sides. When a local block transitions to a consensus block,
it waits to receive the event from the blockchain; sending the method
call first if it is the initiator. (This means that initiators wait
for confirmation from the chain before continuing.)

Our libraries are written in continuation-passing style, so
interaction with the blockchain requires a continuation argument. In
fact, the entire client is written in continuation-passing style, so
the front-end code, authored by Reach users, passes a continuation
into the Reach client, which will be called with the final value
returned from the program. This front-end code is available at
[spec/stdlib/web3/rps-spec.mjs](https://github.com/Reach-sh/reach-lang/blob/master/examples/rps/rps/spec/stdlib/web3/rps-spec.mjs).

Let's take a look at the header of the function for Alice:

```
export function A(stdlib, ctc, interact, v0, v1, v2, kTop) {
```

The function expects to receive (a) the Reach standard library
(provided by the Reach runtime), (b) a handle for the contract
(produced by calling some Reach runtime functions), (c) an
implementation of a function for interacting with the frontend
(produced by the frontend programmer) (d) the initial knowledge of the
participant (`wagerAmount`, `escrowAmount`, and `handA`), then (e) the
continuation of the entire run of the program. When Alice's role is
complete, Reach will call `kTop`. If the Reach program uses the
primitive `interact`, then the provided `interact` function will be
called with a continuation argument to allow the frontend to take
control and direct the computation.

Once this information is provided to Alice's client, it can begin
computing the first local block of Alice:

```
  const v4 = stdlib.le(0, v2);
  const v5 = stdlib.lt(v2, 3);
  const v6 = v4 ? v5 : false;
  stdlib.assert(v6);
  const v10 = stdlib.random_uint256();
  const v11 = stdlib.uint256_to_bytes(v10);
  const v12 = stdlib.uint256_to_bytes(v2);
  const v13 = stdlib.bytes_cat(v11, v12);
  const v14 = stdlib.keccak256(v13);
  const v15 = v0;
  const v16 = v1;
  const v17 = stdlib.add(v15, v16);
```

Then it can initiate the first consensus block:

```
  ctc.send("m0", [v15, v16, v14], v17, () => {
    ctc.recv("e0", (p15, p16, p14, v18) => {
      stdlib.assert(stdlib.equal(v15, p15));
      stdlib.assert(stdlib.equal(v16, p16));
      stdlib.assert(stdlib.equal(v14, p14));
```

As mentioned, the initiating participant first sends the message
(`v14` through `v16`) plus the transfer amount (`v17`), then confirms
the method ran successfully by waiting to receive the corresponding
event. Once the event is received, it ensures that the received event
is the same one that it sent by checking that the values are the same
as those predicted by it (comparing `v15` with `p15` and so on.) In
addition the publication, the `ctc.recv` function exposes the transfer
amount `v18`. At this point, Alice has the same information that
consensus block did, so it runs the exact same code as the consensus:

```
      const v19 = stdlib.add(v15, v16);
      const v20 = stdlib.eq(v18, v19);
      stdlib.assert(v20);
```

At this point, Alice must wait for Bob's event to appear and run the
corresponding consensus block:

```
      ctc.recv("e1", (v21, v22) => {
        const v23 = stdlib.eq(v22, v15);
        stdlib.assert(v23);
        const v24 = stdlib.le(0, v21);
        const v25 = stdlib.lt(v21, 3);
        const v26 = v24 ? v25 : false;
        stdlib.assert(v26);
```

Finally, Alice publishes her last message and then runs the last
consensus block, which performs a lot of computations to compute the
transfer amount:

```
        const v27 = v10;
        const v28 = v2;
        ctc.send("m2", [v14, v15, v16, v21, v27, v28], 0, () => {
          ctc.recv("e2", (p27, p28, v29) => {
            stdlib.assert(stdlib.equal(v27, p27));
            stdlib.assert(stdlib.equal(v28, p28));
            const v30 = stdlib.eq(v29, 0);
            stdlib.assert(v30);
            const v31 = stdlib.uint256_to_bytes(v27);
            const v32 = stdlib.uint256_to_bytes(v28);
            const v33 = stdlib.bytes_cat(v31, v32);
            const v34 = stdlib.keccak256(v33);
            const v35 = stdlib.eq(v14, v34);
            stdlib.assert(v35);
            const v36 = stdlib.le(0, v28);
            const v37 = stdlib.lt(v28, 3);
            const v38 = v36 ? v37 : false;
            stdlib.assert(v38);
            const v39 = stdlib.le(0, v28);
            const v40 = stdlib.lt(v28, 3);
            const v41 = v39 ? v40 : false;
            const v42 = stdlib.le(0, v21);
            const v43 = stdlib.lt(v21, 3);
            const v44 = v42 ? v43 : false;
            const v45 = v41 ? v44 : false;
            const v46 = stdlib.sub(4, v21);
            const v47 = stdlib.add(v28, v46);
            const v48 = stdlib.mod(v47, 3);
            const v49 = v44 ? 0 : 1;
            const v50 = v41 ? 2 : v49;
            const v51 = v45 ? v48 : v50;
            kTop(v51); }); }); }); }); }); }
```

---

The code for Bob is similar, except dual: where Alice sends, Bob
receives and vice versa. All together, Bob's code is 46 lines and
Alice's code is 60 lines. It would be tedious to produce this code
manually, because there is so much duplication of computation between
each of the sides and the contract. Any inconsistency between these
three programs is a potential error and opening for an attack on the
resources controlled by the application. Reach reduces the required
engineering effort and increases the reliability and trustworthiness
of the entire application by removing inconsistencies between the
various pieces of software from the attack surface.

# Future Work

The fundamentals of Reach are all in place, but there remains more
work to be done for it to be sufficient for all decentralized
applications.

Although Reach's model is blockchain agnostic, we currently only
target Ethereum. We are talking to many other layer-1 providers to
form partnerships and build more backends.

Presently, our Ethereum backend generates Solidity code rather than
bytecode directly. Given that we use such a restricted form of
Solidity, we intend to generate EVM bytecode and take over
optimization of the code directly, using the extra information only
our compiler has access to.

The computational fragment of Reach is quite limited, with a small
number of types and operations. We intend to extend the type system
and standard library to incorporate more functionality. We will
add simply-typed functions (to ensure termination) and loops with
provable bounds.

The aforementioned limitations are fundamentally engineering problems
where we understand the solutions and need to spend the time and money
to develop them. There are, however, more interesting theoretical
problems we are working on next. Each of these involves relaxing one
of the constraints on the Reach computation model.

Many DApps do not have a single designated next action in all cases,
but instead offer a choice between two different continuations,
including different actors in each. Reach does not support
`CHOICE`-style interactions. The fundamental challenge is ensuring
that when Alice has a choice between two options, Bob can reliably
learn which Alice will chose. This suggests that choices are always
expressed in terms of multiple next consensus blocks, perhaps with
differing requirements. In the context of our example, we would want
to allow Alice to recover her deposit if Bob refuses to play after
some timeout.

In Reach, participants represent particular keys on the blockchain
we deploy to. The set of participants is fixed at the beginning of the
program and embedded into the protocol state. Most DApps do not
involve a predetermined set of participants, but instead involve a
dynamically known set of participants drawn from some set of
participant classes. For example, a blackjack game involves the house
and a set of players. The main problem this presents for the Reach
model is that Alice may wish to post a block, but is unable to because
Eve posted first. We need to update the Reach model to allow Alice
to update her block based on the new state resulting from Eve's and
try again. It is likely that this is a special case of a `WHILE` and a
`CHOICE`.

In the longer term, we are interested in exploring the semantics of
decentralized applications that concurrently operate on multiple
consensus chains, rather than a single network, and only partially
share information, rather than only distinguishing between `Public`
and `Secret`.

Finally, Reach is implemented as an approximately 2,500 line
Haskell program that must be trusted for the claims made about
Reach programs to themselves be trustworthy. In the future, we
intend to rewrite the compiler in a verified language, like Coq or
Agda, and prove that its transformations are semantics-preserving.

# Conclusion

Reach is a new domain-specific language specialized for trustworthy
decentralized applications. Its blockchain-agnostic model frees
developers from lock-in to a specific platform. Its verification
strategy increases the reliability and trustworthiness of Reach
programs over traditionally developed DApps. Reach's use of
End-Point Projection ensures that on-chain and client-side
computations are synchronized and agree on all fundamental parts of
program operation.

Although Reach is usable today, it is a work-in-progress with room
for growth and development. You can start using it today by visiting
[Reach-sh/reach-lang](https://github.com/Reach-sh/reach-lang).
