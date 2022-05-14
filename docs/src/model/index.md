# {#ref-model} Model

This document describes the fundamental assumptions and concepts of Reach. First, we discuss the model of running a Reach program in @{seclink("ref-model-eval")}. Next, we discuss the details about compilation of a Reach program that are relevant to Reach programmers in @{seclink("ref-model-compile")}. Finally, we discuss how Reach programs are syntactically constructed in @{seclink("ref-model-syntax")}.

:::note
This is not an introduction to Reach.
We recommend reading [the overview](##overview) for an introduction to what Reach is and the [tutorial](##tut) to get started with programming Reach.

Furthermore, it is not an introduction to consensus networks or "blockchain".
If you would like to read such an introduction, we recommend the [Wikipedia article on consensus](https://en.wikipedia.org/wiki/Consensus_(computer_science)) and the [Wikipedia article on blockchains](https://en.wikipedia.org/wiki/Blockchain).
:::

## {#ref-model-eval} Evaluation Model

Reach programs specify a decentralized application (@{defn("DApp")}), which is a distributed computation involving many participants and utilizing one contract on one consensus network for reaching agreement on the intermediate values of the computation.
:::note
"Many" is a technical term that means "zero or more".
:::

When the computation terminates, all participants agree on the outcome, because they agreed on the intermediate values.

At the start of a Reach computation, the set of participants is not necessarily known and can evolve throughout the execution of the application.

A @{defn("consensus network")} is a network protocol with a network token, a set of non-network tokens, a set of accounts, a set of contracts, and a network time.
A @{defn("network token")} is an opaque unit of account.
A @{defn("non-network token")} is an opaque unit of account;
typical consensus networks allow the set of non-network tokens to grow over time and be issued by accounts.

:::note
Reach assumes that network tokens and non-network tokens behave identically, but often they do not; [this article](##guide-nntoks) discusses the causes and consequences of this.
:::

A consensus network's @{defn("network time")} is some monotonically increasing discrete value from a totally ordered set; typically, it is the height of the blockchain for consensus networks that use blockchains.
A @{defn("time delta")} represents the difference between two points in network time as a discrete number of network time units.
Each network time corresponds to some @{defn("network second")}, which has a connection to real time; typically, it is a [Unix time](https://en.wikipedia.org/wiki/Unix_time).

:::note
This description of consensus networks is an abstraction that may not be directly implemented by actual networks.

For example, in UTXO-based networks, there is not typically an explicitly represented account balance ledger.
However, such networks do _abstractly_ have accounts with balances, because particular private keys represent accounts which have exclusive access to some set of network tokens which is their balance.

Similarly, Reach's notion of time may appear overly abstract ("monotonically increasing ... totally ordered set") if you know that many consensus networks are based on blockchains and use the chain length, also called the height or block number, as a notion of time.
In this case, network time would be a natural number, which is a prototypical model of a totally ordered set.
However, Reach is flexible enough to support non-blockchain-based consensus networks, so it does not mandate this particular natural number-based notion of time.

Finally, Reach's definition of consensus network does not require any particular technology or features of this.
In particular, it does not only refer to so-called "layer-1" protocols, nor does it exclude centralized systems with trusted parties controlling the network.
:::

Consensus networks support @{defn("transfers")} of network tokens and non-network tokens between accounts.
An @{defn("account")} is a unique identity (called an @{defn("address")}) with a non-negative balance of network tokens.
Accounts may sign values in a way that may not be repudiated or impersonated; this is called @{defn("publication")}.
The chapter, @{seclink("ref-networks")}, discusses which consensus networks are supported by Reach.

@{defn("Contracts")} are accounts with three extra capacities: they persistently store values (called the @{defn("consensus state")}), they may receive publications, and when they receive publications, they systematically process them and may modify their consensus state, make publications, and may transfer network tokens and non-network tokens in response to the reception.
In addition to values, consensus state may contain a fixed number of @{defn("mappings")} between an address and a value.
These mappings are referred to as "@{defn("linear state")}" because their size is linear in the number of addresses visible to the contract.
Furthermore, a contract may provide @{defn("views")} of its consensus state, which are labeled functions and values which may be hierarchically organized, such as `NFT.owner` or `scoreOfPlayer`.
These views are visible in sub-trees of the computation graph.
A contract may also emit @{defn("event")}s, which are externally observable values that are persistently available.
On some networks, contracts have @{defn("companion")}s, which we are another contract that does work for them that cannot be done in the contract itself.
The creation of a contract is called @{defn("deploy")}ment.

A @{defn("participant")} is a logical actor which takes part in a DApp.
It is associated with an account on the consensus network.

:::note
The same account may be used by multiple participants in a DApp.
:::

A participant has persistently stored values, called its @{defn("local state")}. It has a frontend which it interacts with. A @{defn("frontend")} is an abstract actor which supports a set of functions which consume and produce values; when a participant invokes one of these functions it is referred to as @{defn("interact")}ion.

A @{defn("participant class")} is a category of participant that may occur many times in a single application.
Members of a participant class are referred to as @{defn("participant instances")} when their status as a member of a class is important, but just "participants" otherwise.
Participant instances are independent participants like any other; for example, with their own local state, frontend, and so on.
The main distinction is that when a member of a participant class joins an application, it is not fixed like other participants, because a participant instance does not exclusively represent the participant class.

An @{defn("API")} is a source of publications that do not correspond to any participant and are therefore like asynchronous events that impinge on the computation.
The contract returns a value to an API call.
APIs are organized into a labeled hierarchy, like `Contest.vote` and `User.write`.

Since DApps have an associated contract, they have an associated account.

:::note
The contract account must be distinct from all participant accounts.
:::

This account is assumed to be empty when the computation starts.

:::note
On some consensus networks, it is possible for transfers to a contract account to occur outside of the purview of Reach. If this occurs, then those network tokens are remitted to the originator of the final consensus transfer.
:::

Any network tokens transferred into the account must be removed by the DApp's completion. This is called the @{defn("token linearity property")}.

A DApp computation can be seen as a graph of steps with a unique first step. A @{defn("step")} is a set of local steps by participants followed by a single consensus step introduced via a single consensus transfer.

A @{defn("local step")} is executed by a single participant and is a sequence of local computations. A @{defn("local computation")} may bind a piece of local state, assert a property of the local state, or interact with the frontend. A @{defn("consensus transfer")} is executed by a single participant (called the @{defn("originator")}) which makes a publication of a set of public values from its local state and transfers zero or more network tokens to the contract account. A consensus transfer specifies an alternative step, called a @{defn("timeout")}, that is executed if the originator fails to make the transfer before a given time delta has elapsed. All local state is initially @{defn("private")}, until it is explicitly made @{defn("public")} via a @{defn("declassification")}, which is a kind of local computation.

A participant is said to @{defn("join")} an application when it first makes a publication.
For non-participant instances, this also makes the participant @{defn("fixed")}, whereby the consensus state includes an assignment from the participant to the particular account (i.e. address) which it is fixed to.
All subsequent publications by a fixed participant must be from the fixed account.

A @{defn("consensus step")} is a graph of consensus computations with a unique first computation.
A @{defn("consensus computation")} either binds consensus state,
asserts a property of the consensus state,
performs a transfer,
selects between different next consensus computations,
communicates with another contract (referred to as a remote object),
or @{defn("commits")} to the next step.

An @{defn("assert")}ion is either: a @{defn("knowledge assertion")}, which is a claim that one honest participant cannot know something that another honest participant does know; a @{defn("static assertion")}, which is an always-true formula; an @{defn("assumption")}, which is a true formula if frontends behave honestly; a @{defn("requirement")}, which is a true formula if participants behave honestly; or, a @{defn("possibility assertion")}, which is a formula for which there exists some values that honest participants and frontends could submit which results in the truth of the formula.
An @{defn("honest")} participant is one that executes the steps specified by the DApp, while an honest frontend is one that only returns values which ensure that all assumptions evaluate to the boolean `{!rsh} true`.

A @{defn("value")} is either: the `null` value, a boolean, an unsigned integer, a string of bytes, a digest, an address, a fixed tuple of values, a statically-sized homogeneous array of values, or a fixed record of values.
Values may be @{defn("digest")}ed to produce a [cryptographic hash](https://en.wikipedia.org/wiki/Cryptographic_hash_function) of their binary encoding.

Values are in one of three possible conditions.
They could be consensus state, in which case they are known to all participants.
They could be local state of a single participant, which means they are known by only that participant.
Local state is further divided into private local state, which cannot be included in a publication, and public local state, which can.
These conditions are summarized thus:
+ **Local, private**: The initial state.
+ **Local, public**: The result of `{!rsh} declassify`.
+ **Consensus**: The result of `{!rsh} publish`.

## {#ref-model-compile} Compilation Model

Reach programs cannot execute independently of a consensus network and a set of frontends.
Thus, the semantics of Reach treats these components abstractly and does not specify their semantics.
Therefore, the semantics of Reach cannot be effectively implemented directly in a virtual machine or interpreter.
Instead, Reach programs are @{defn("compile")}d to
a particular consensus network @{defn("connector")}
and a set of participant @{defn("backends")}
which execute the computation of the particular consensus network.
Connectors and backends are sound
if they faithfully model the abstract semantics assumed by Reach.

During compilation, the Reach compiler automatically verifies that the token linearity property and all static assertions and possibility assertions are true whether participants and frontends are honest or not.
Similarly, all knowledge assertions are verified using a conservative approximation of participant knowledge.
This conservative approximation assumes that all inputs to a computation are revealed by the result of the computation, except for digests and interaction.
This approximation means that Reach cannot, for example, reason about the details of manually expressed encryption formulas and will assume they are insecure.
Finally, a subtle point about the knowledge checker is relevant: technically participants with different identities in a Reach program may actually be instantiated by the same principals, i.e. if Alice choses to play a game of a Chess against herself, where she controls both Black and White; as this is always possible, the knowledge checker does not consider it a violation of a claim that White knows something Black does not.

If these assertions cannot be statically verified, then the compilation process aborts.
After this verification, such static assertions and possibility assertions are removed from the program and do not occur at runtime.
In contrast, assumptions are enforced at runtime by backends and requirements are enforced at runtime by connectors.
If assumptions are violated at runtime, then the backend aborts.
If requirements are violated at runtime, then the connector ensures that all aspects of the DApp (the contract and participant) ignore the inducing consensus transfer, which often results in a timeout.

## {#ref-model-syntax} Syntax Model

Reach programs are specified via a subset of well-formed JavaScript syntax inside source files. The section @{seclink("ref-programs")} describes the syntax of Reach programs in detail.
