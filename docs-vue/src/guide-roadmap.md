


# {#guide-roadmap} Reach's Roadmap

This section describes in a vague way some of the bigger future plans for Reach's development.
These are vague for expediency, but we're willing to elaborate if you ask in <CommunityLink />.
We welcome your contributions on [GitHub](https://github.com/reach-sh/reach-lang) and in <CommunityLink /> to help bring these plans to fruition.

Last updated: 2021/06/30

**Short term**:
+ General - Apple Silicon support
+ Language - `race`-winner `only` blocks
+ General - Error code index
+ Language - `fork`/`parallelReduce` local pass-through values
+ Language - Stateless participants
+ Language - `interact` continuations
+ Networks - Explicit state compilation option
+ Networks - Participant fast catch-up
+ Networks - [Conflux](https://confluxnetwork.org/)
+ Networks - Algorand - [AVM 0.9](https://forum.algorand.org/t/testnet-and-mainnet-update-2-7-1/3647) support
+ Networks - Algorand - Post-AVM limitation removal - remote objects, token minting, arbitrary contract length, etc
+ Optimization - Unify view functions with identical bodies
+ Language - State linear in the number of transactions, rather than only participants
+ Testing - Unified devnet rather than per-application devnet
+ Frontends - Expose TypeScript types
+ Frontends - Session resumption
+ Language - Output streams
+ Networks - Interface constraints
+ General - Reach debugger


**Medium term**:
+ IDE - Language Server Protocol implementation
+ Language - threading / futures
+ Language - `for` to `while` syntactic sugar
+ Language - tail-recursive function to `while`
+ Language - fixed range integer types
+ Optimization - data-type coallescing
+ Language - non-communicating guaranteed termination loops
+ Verification - Constrain eventual use of values
+ Verification - `exit()` reachability
+ Verification - network analysis
+ Language - Dynamic token tracking


**Long term**:
+ Language - [substructural types](https://en.wikipedia.org/wiki/Substructural_type_system) to allow mutation
+ Language - pay-as-you-go closures
+ Language - general recursion through closure conversion of non-contifiable continuations
+ Verification - game-theoretic property verification
+ Verification - verified compiler


