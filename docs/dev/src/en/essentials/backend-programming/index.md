---
menuItem: mi-docs
---

# Backend Programming

This chapter focuses on Reach Programming which uses the Reach programming language to write code in *index.rsh*, and the [Reach Tool](/en/essentials/backend-programming/reach-tool/) to (among other things) compile *index.rsh* into *index.main.mjs*:

<div><img src="backend-programming.png" class="img-fluid" width=600 height=367 loading="lazy"></div>

The following sections describe the fundamental concepts and assumptions of Reach.

## Reach Programs

Reach programs specify a decentralized application (DApp), which is a distributed computation involving many participants and utilizing one contract on one consensus network for reaching agreement on the intermediate values of the computation. When the computation terminates, all participants agree on the outcome, because they agreed on the intermediate values. At the start of a Reach computation, the set of participants is not necessarily known and can evolve throughout the execution of the application.

## Consensus Networks

A consensus network is a network protocol with a network token, a set of non-network tokens, a set of accounts, a set of contracts, and a network time. A network token is an opaque unit of account. A non-network token is an opaque unit of account; typical consensus networks allow the set of non-network tokens to grow over time and be issued by accounts. A consensus network’s network time is some monotonically increasing discrete value from a totally ordered set; typically, it is the height of the blockchain for consensus networks that use blockchains. A time delta represents the difference between two points in network time as a discrete number of network time units. Each network time corresponds to some network second, which has a connection to real time; typically, it is a Unix time. Consensus networks support transfers of network tokens and non-network tokens between accounts. An account is a unique identity (called an address) with a non-negative balance of network tokens. Accounts may sign values in a way that may not be repudiated or impersonated; this is called publication. The chapter, Consensus Network Connectors, discusses which consensus networks are supported by Reach.

## Contracts

Contracts are accounts with three extra capacities: they persistently store values (called the consensus state), they may receive publications, and when they receive publications, they systematically process them and may modify their consensus state, make publications, and may transfer network tokens and non-network tokens in response to the reception. In addition to values, consensus state may contain a fixed number of mappings between an address and a value. These mappings are referred to as "linear state" because their size is linear in the number of participants in the contract. Furthermore, a contract may provide views of its consensus state, which are labeled functions and values which may be hierarchically organized, such as NFT.owner or scoreOfPlayer. These views are visible in sub-trees of the computation graph. The creation of a contract is called deployment.

## Participants

A participant is a logical actor which takes part in a DApp. It is associated with an account on the consensus network.
The same account may be used by multiple participants in a DApp. A participant has persistently stored values, called its local state. It has a frontend which it interacts with. A frontend is an abstract actor which supports a set of functions which consume and produce values; when a participant invokes one of these functions it is referred to as interaction.

## Participant Classes

A participant class is a category of participant that may occur many times in a single application. Members of a participant class are referred to as participant instances when their status as a member of a class is important, but just "participants" otherwise. Participant instances are independent participants like any other; for example, with their own local state, frontend, and so on. The main distinction is that when a member of a participant class joins an application, it is not fixed like other participants, because a participant instance does not exclusively represent the participant class.

## APIs

An API is a source of publications that do not correspond to any participant and are therefore like asynchronous events that impinge on the computation. The contract returns a value to an API call. APIs are organized into a labeled hierarchy, like Contest.vote and User.write.

## Token Linearity

Since DApps have an associated contract, they have an associated account. This account is assumed to be empty when the computation starts. Any network tokens transferred into the account must be removed by the DApp’s completion. This is called the token linearity property.

## Steps

A DApp computation can be seen as a graph of steps with a unique first step. A step is a set of local steps by participants followed by a single consensus step introduced via a single consensus transfer.

## Local Steps

A local step is executed by a single participant and is a sequence of local computations. A local computation may bind a piece of local state, assert a property of the local state, or interact with the frontend. A consensus transfer is executed by a single participant (called the originator) which makes a publication of a set of public values from its local state and transfers zero or more network tokens to the contract account. 

## Consensus Transfers

A consensus transfer specifies an alternative step, called a timeout, that is executed if the originator fails to make the transfer before a given time delta has elapsed. All local state is initially private, until it is explicitly made public via a declassification, which is a kind of local computation.

## Fixed Participants

A participant is said to join an application when it first makes a publication. For non-participant instances, this also makes the participant fixed, whereby the consensus state includes an assignment from the participant to the particular account (i.e. address) which it is fixed to. All subsequent publications by a fixed participant must be from the fixed account.

## Consensus Steps

A consensus step is a graph of consensus computations with a unique first computation. A consensus computation either binds consensus state, asserts a property of the consensus state, performs a transfer, selects between different next consensus computations, communicates with another contract (referred to as a remote object), or commits to the next step.

## Assertions

An assertion is either: a knowledge assertion, which is a claim that one honest participant cannot know something that another honest participant does know; a static assertion, which is an always-true formula; an assumption, which is a true formula if frontends behave honestly; a requirement, which is a true formula if participants behave honestly; or, a possibility assertion, which is a formula for which there exists some values that honest participants and frontends could submit which results in the truth of the formula. An honest participant is one that executes the steps specified by the DApp, while an honest frontend is one that only returns values which ensure that all assumptions evaluate to the boolean true.

## Values

A value is either: the null value, a boolean, an unsigned integer, a string of bytes, a digest, an address, a fixed tuple of values, a statically-sized homogeneous array of values, or a fixed record of values. Values may be digested to produce a cryptographic hash of their binary encoding. Values are in one of three possible conditions. They could be consensus state, in which case they are known to all participants. They could be local state of a single participant, which means they are known by only that participant. Local state is further divided into private local state, which cannot be included in a publication, and public local state, which can. These conditions are summarized in figure 16.

## Compilation

Reach programs cannot execute independently of a consensus network and a set of frontends. Thus, the semantics of Reach treats these components abstractly and does not specify their semantics. Therefore, the semantics of Reach cannot be effectively implemented directly in a virtual machine or interpreter. Instead, Reach programs are compiled to a particular consensus network connector and a set of participant backends which execute the computation of the particular consensus network. Connectors and backends are sound if they faithfully model the abstract semantics assumed by Reach.

During compilation, the Reach compiler automatically verifies that the token linearity property and all static assertions and possibility assertions are true whether participants and frontends are honest or not. Similarly, all knowledge assertions are verified using a conservative approximation of participant knowledge. This conservative approximation assumes that all inputs to a computation are revealed by the result of the computation, except for digests and interaction. This approximation means that Reach cannot, for example, reason about the details of manually expressed encryption formulas and will assume they are insecure. Finally, a subtle point about the knowledge checker is relevant: technically, participants with different identities in a Reach program may actually be instantiated by the same principals, i.e. if Alice choses to play a game of chess against herself, where she controls both Black and White; as this is always possible, the knowledge checker does not consider it a violation of a claim that White knows something Black does not.

If these assertions cannot be statically verified, then the compilation process aborts. After this verification, such static assertions and possibility assertions are removed from the program and do not occur at runtime. In contrast, assumptions are enforced at runtime by backends and requirements are enforced at runtime by connectors. If assumptions are violated at runtime, then the backend aborts. If requirements are violated at runtime, then the connector ensures that all aspects of the DApp (the contract and participants) ignore the inducing consensus transfer, which often results in a timeout.

## Syntax

See [Reach Basic](/en/essentials/backend-programming/reach-basic/).