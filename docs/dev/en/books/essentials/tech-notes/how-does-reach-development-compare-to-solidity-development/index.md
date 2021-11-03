---
author: Jay McCarthy
menuItem: mi-docs
publishedDate: 2020-08-29T14:00:00
---

# How does Reach development compare to Solidity development?

The Reach documentation is written from the perspective of a developer who has never done any DApp development of any kind before starting to use Reach. However, mastering Reach development is a valuable skill for those developers with experience using tools like [Solidity](https://soliditylang.org). But, when these developers read the Reach documentation, they are often at loss to see how what they're reading relates to what they already know. This article attempts to bridge that gap and help Solidity-style developers understand Reach.

For the rest of this article, when we use the term "Solidity development", we're referring to blockchain development without Reach and not literally only the programming language Solidity. For example, "Solidity development" includes [Vyper](https://github.com/vyperlang/vyper) developers, as well as users of tools like [Truffle Suite](https://www.trufflesuite.com), [web3.js](https://web3js.readthedocs.io/), and so on. We're talking about a paradigm of DApp programming and not a specific tool.

## Solidity Development

A great Solidity project typically includes seven components:

1. **Protocol Design** --- A DApp starts with a protocol diagram, perhaps written using a pidgin [UML](https://en.wikipedia.org/wiki/Unified_Modeling_Language) format, where a developer thinks about the state space of an application and the valid transitions in that state space.
This diagram includes annotations about typical use-cases and workflows for particular paths through the protocol.
2. **Smart Contract** --- A developer studies the protocol design and defines a smart contract program whose implicit state space matches the protocol.
For example, if a protocol has two nodes, `A` and `B`, connected by a transition labeled `f`, then the smart contract might have a Boolean `storage` variable with a name like `inA` and a method named `f` that checks if that variable is `true` and modifies it to be `false`---thus `A` in the diagram corresponds to `inA = true` and `B` in the diagram corresponds to `inB = false`.
Part of the challenge of smart contract development is understanding the connection between the implicit state of the smart contract and the original protocol design.
3. **Middleware** --- A developer builds a library in a language like JavaScript or Go that uses an SDK like `web3.js` or `ethers` to connect to their smart contract.
This middleware is co-developed with the smart contract and duplicates details like the names of methods and their arguments.
It abstracts these details into higher-level patterns of operation that roughly correspond to the use-cases and workflows in the protocol design.
4. **Frontend** --- A developer builds a user interface that connects to the middleware and potentially a wallet, like MetaMask, and provides a high-level user-facing perspective on the software.
This is mostly insulated from the details of the particular protocol and contract, but is typically specialized to the consensus network, because the user interface surfaces details about the underlying network.
5. **Testing** --- Developers typically use tools like Truffle Suite or a developer instance of `geth` to launch a test consensus network and then build a suite of unit and end-to-end tests of their application, typically by either directly interacting with the middleware or the smart contract itself.
This same testing environment may be used to back a sample version of the frontend.
6. **Verification** --- High quality DApps are verified and audited for a variety of properties, from basic checking of things like integer and buffer overflows, to checking of the absence of famous attacks like reentrancy attacks, to more thorough checking of properties related to the particular domain of the application.
There are a wide variety of tools and companies that provide this kind of verification as a service and the best DApps are not deployed until they pass these tests, sometimes multiple of them.
Verification typically involves only the smart contract and the best versions often require a mechanical representation of the protocol design to analyze the behavior of the DApp across all its workflows.
7. **Deployment** --- Once the DApp is ready to be released, it will either be embedded in the frontend, if users are expected to launch their own independent instances of the smart contract, or it is launched a single time (perhaps as a contract factory) at a well-publicized address that might be embedded in the frontend.


Obviously, we've left out a lot of details, but this is a sketch of the typical components of Solidity-style development.
There are a huge number of options and techniques for almost all of these components, with some being dropped or minimized depending on the needs of the particular application.
For example, an extremely simple DApp might combine the middleware and frontend or embed a description of the protocol design as comments or some ASCII art in the smart contract source code.

## Reach Development

Reach development includes each one of these seven components. But rather than requiring the use of a variety of different tools, the difficulties associated with each component are solved by a different aspect of Reach.

1. **Protocol Design** --- As a programming language, Reach operates at a different level of abstraction than a language like Solidity.
Reach programs encode the same information that would be in the informal protocol design diagrams that often accompany smart contracts.
A Reach program specifically names the various participants in a DApp and their individual workflows as a single chronological workflow that shows how and when the various individual workflows intertwine.
2. **Smart Contract** --- By operating at this higher-level of abstraction, Reach developers are not responsible for determining the state space of the smart contract.
Instead, the Reach compiler analyzes the structure of communication in the protocol design and derives the necessary state and transition functions.
This doesn't mean that Reach developers don't think about "consensus"; indeed, the consensus step is a fundamental part of a Reach program.
However, Reach developers can focus on the constraints on individual publications and the consensual actions of the computation, rather than the mechanics of ensuring the smart contract is in the appropriate state and transitions to the correct next state.
3. **Middleware** --- Similarly, a Reach program includes a specification of the API of the middleware layer, via a participant interact interface.
This part of a Reach program is particular to each participant and explicitly names the points of contact between the participant workflow (embedded in the Reach program) and the frontend.
This means that Reach developers do not need to manually keep the middleware and smart contract in sync or update either as the protocol design changes: the Reach compiler does all of that for them.
4. **Frontend** --- Reach programs do not embed the frontend, like they embed the protocol design, smart contract, and middleware.
Reach developers design and build user interfaces just like they do in Solidity-style development, except that they tend to have an easier time, because the automatically-generated middleware layer thoroughly insulates them from the low-level details of the consensus network.
Reach developers can easily build their interface with JavaScript using the Reach standard library; soon, they will be able to use the language of their choice using the Reach RPC server.
5. **Testing** --- Reach facilitates testing in two ways.
First, the Reach language embeds a high-quality property-based testing system via the `assert` and `forall` primitives, which can be used in a basic way to write simple test cases.
Second, the Reach deployment tool, `reach run`, allows for the easy construction and running of automated test suites for the workflows of a DApp.
In both cases, it is not necessary for Reach developers to directly manage their own development networks or otherwise interact with the consensus networks they're testing with in any way.
6. **Verification** --- Every Reach compilation includes a thorough formal verification of the DApp using an SMT-based theorem prover.
This theorem prover verifies general properties that all programs should exhibit, such as never overflowing finite memory bounds or accessing uninitialized memory.
It verifies properties that all DApps should exhibit, such as token linearity property which guarantees that funds are not double-spent or forgotten about.
Furthermore, it verifies bespoke properties that are unique to the particular DApp, as specified with the `assert` primitive.
7. **Deployment** --- Some aspects of deployment decisions are embedded inside of Reach programs, such as whether to use a contract factory or whether to have each instance of the DApp use an independent smart contract deployed by one of the participants.
Other aspects are part of the configuration of the Reach compiler, such as which consensus network will be targeted.
Still others are part of the configuration of the Reach standard library and testing infrastructure, like which wallet the middleware should connect to or which kind of development node should be launched and managed by Reach.

## Conclusion

In summary, a vibrant ecosystem of many tools, techniques, and traditions have grown up around the creation of DApps in the Solidity-style.
Reach leverages the experience of that ecosystem and provides a total solution that incorporates all of the different components into one environment, so that it can provide more services with high quality and lower cost by integrating them together.