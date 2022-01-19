# {#guide-limits} What are Reach's limitations?

Today, Reach is a powerful language for building decentralized applications, as demonstrated in [the overview](##overview), [the tutorial](##tut), and the [workshop series](##workshop).
However, it has a lot of potential for growth.
This section describes a few of these areas and gives brief sketches of our roadmap for directing this growth.
We welcome your contributions on [GitHub](https://github.com/reach-sh/reach-lang) and in [the Discord community](@{DISCORD}) to help bring these plans to fruition.

---
**Connectors.** Foremost, Reach is a consensus network-agnostic language, so one of our highest priorities is supporting a wide variety of platforms, including layer-2 abstractions over other layer-1 networks.
Presently, we have a robust [Ethereum backend](##ref-network-eth) and [Algorand backend](##ref-network-algo).

---
**Backends.** Presently, Reach has a robust backend for JavaScript that is well-suited for client-facing applications and JavaScript servers.
However, we believe that many decentralized application developers would like to make use of languages like Go and Rust for their participants.
Presently, this can be accomplished via the [RPC server](##ref-backends-rpc), but we'd like to build a dedicated backend for languages like these.

---
**Computation.** Reach's computational language is based on JavaScript and contains many of JavaScript's most desirable features, like arrow expressions, free-form objects, destructuring bindings, robust `{!rsh} import` and `{!rsh} export` specificiers, and so on.
However, there are some differences that represent limitations, such as the inability to use functions as values at runtime and the need to enforce finite limits on data.

---
**Verification.** Reach's verifier is robust in the face of many complex and interesting theorems about decentralized application behavior, but it is inherently conservative and does not presently allow users to manually prove theorems that are conservatively rejected.

---
**Network Integration.** Since Reach is consensus network-agnostic, it is not possible for Reach programs to directly integrate with network-specific features, such as observing the blockhash on Ethereum.
Reach programs can instead interact with these low-level details of their chosen consensus network via remote object interaction.

---
**Communication.**
Reach's communication language has some limitations that we have plans to remove, which are discussed in [roadmap](##guide-roadmap), but there are some for which we do not have plans to remove.
For example, we do not intend to support co-inductive or cyclic state, nor expose an arbitrary consensus heap to programmers.