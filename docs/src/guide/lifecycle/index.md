# {#guide-lifecycle} Contract Development Lifecycle and Best Practices for Building Reach DApps

Smart contracts impose unique restrictions on development that is not experienced in web2. 
Building web3 DApps with intentionality enables efficient use of a developer's time and reduces exposure to unforeseen vulnerabilities. 

## Key Ideas and Goals

Before any code is created, take time to answer the following questions to better understand the problem your DApp will solve.
Start with the 'Why'. 

* **Why** do we need a DApp?
  * What is the motivation to create this DApp?
  * Is decentralization adding real value for users? 
  * Could it be on a centralized database instead of blockchain?
  * Is there a revenue model? Is it fair? Will users agree?
* **How** do we plan to achieve the DApp's goal?
  * How do we position the DApp to our customers or users?
  * How will this DApp solve problems?
* **What** will the DApp do?
  * What specific problem will you solve?
  * What does the DApp need to accomplish?
  * What is the minimum viable product (MVP)?
  * Which products or services are we offering that require a new contract?
  * Can it be more decentralized to add even more value for users? Is there adequate incentive (profit, reputation, other) to do it that way?
* **Where** will it be deployed?
  * Which consensus networks will this DApp live on?
  * Will the DApp rely on premium indexer services or free tiers of service?
* **Who** is our user persona?
  * Is the persona documented?
  * Who is it trying to impress and why?
  * Who will find value in using this DApp? What pain will it solve?
  * Will incentives align for people to use the DApp in the intended manner?
  * Does this DApp fulfill grant requirements, appeal to certain investors, or potential partner networks?
* **When** should it be delivered?
  * Does this deadline have a high opportunity cost?

Take time to create answers that are as specific as possible. 
DApps with a clear roadmap are more likely to succeed.

## Requirements and Limitations

Determine the requirements and limitations of the smart contract. 
Requirements should be measurable, specific, and solution-oriented. 
The initial product should be narrow in scope; fight scope-creep by storing new feature suggestions in a backlog.

Once you understand the purpose of the DApp, you can clarify who will interact with the DApp and what restrictions each participant will experience. 
This can be thought of as the communication model of the application. 

* Describe the participants (human or bot):
  * How they will interact with the contract
  * What will they cause the contract to do?
  * What information will the contract return?

Identify specific participants that will cause the contract to do things:

+ A Deployer
+ One or more Admins
  * Is the Deployer the only Admin? 
  * Can Admins change over time?
+ `{!rsh} API`s who add resources to the contract
+ `{!rsh} API`s with special access to extract resources from the contract
  * What must be true to enable this functionality?
+ `{!rsh} API`s who interact with the contract in a completely unprivileged manner

Identify the circumstances that must be met to allow these interactions.

+ Describe information that the contract should expose about itself:
  * Views: Information about the DApps' current state
    * Expose one large informative view that shares as much about the global state as possible, because this can minimize API calls and limit the size of the contract code.
  * `{!rsh} Events`: Information about transaction history and triggered behavior
    * Generally, `{!rsh} Events` should be paired with API calls and only emit associated inputs and outputs.

+ Describe the lifecycle of the contract:
  * Does it live forever?
    * If not, under what circumstances does it end?
  * How many parties are involved in setting things up?
    * What is their expected user experience?
  * When is each API valid?

+ Describe the rules and expectations of the contract:
  * Provide sample inputs and their expected outputs
  * How 'caveat emptor' is the contract?
    * Does it allow users to do foolish things as long as they would only harm themselves in doing so?
    * e.g. Does deleting their user state cause them to lose access to their funds?

+ Based on the requirements above, describe the overall complexity of the contract.
  * How difficult will the contract be to audit?
  * What prior information will auditors need to understand?

Properly prepared requirements should be encapsulated within Reach `{!rsh} API`s, `{!rsh} View`s, `{!rsh} Events`, and `{!rsh} Participant`s. 
Accurate descriptions will allow the smart contract and the UI to be developed simultaneously by coding against the same expectations.

## Determine UI Requirements

* Describe the user experiences that should be possible via the UI
* Describe the kinds of information needed to render each view
  * How many contract views need to be accessed to render the full page?
  * Does this amount scale up over time?
  * How will the UI keep requests under control?
    * Pagination
    * Paying for premium indexer services
* Describe how the UI should limit a user's interactions with the contract, above and beyond the limitations imposed by the contract itself
  * Why is the interaction in the UI instead of in the contract?
* Describe how the UI should surface errors to the user
* How will the UI prevent the user from taking impossible actions
* Describe what happens if: 
  * The indexer service is down or has an inconsistent connection
  * The user doesn't have enough funds
    * There may be different error messages based on the issue
      * Did the user have enough USDCa for the API call, but not enough ALGO to pay the transaction fee?
  * There are DApp-specific reasons that an API call might be rejected by the contract

### Choosing a Frontend

You may choose to use any frontend language or framework to interact with the Reach smart contract. 
However, Reach provides the ability to quickly create React scaffolding with `{!cmd} reach react`

* If you do not have a frontend preference then we suggest using React with Reach

## Developing the Smart Contract

Now that the communication model of the application is clearly understood, you are able to codify the `{!rsh} API`s, `{!rsh} View`s, `{!rsh} Events`, and `{!rsh} Participant`s. 
Use the `{!rsh} interact` interface to dictate how users interact with the contract and other `{!rsh} Participant`s. 
The App Initialization mode should be written to be platform-agnostic. 
Any platform-specific elements should be implemented via the appropriate `{!rsh} interact` interface. 

### General Tips

The following best practices in contract design create Reach DApps that are efficient and faster to debug:
* Prefer to name the Reach file `index.rsh`
* In most cases, prefer `{!rsh} API`s over `{!rsh} Participant`s
* Always include `{!rsh} invariant`s about the contract balance(s) for all currencies in a `{!rsh} while` loop
  * Every time a theorem fails within a `{!rsh} while` loop your first impulse should be to add an `{!rsh} invariant`, not a `{!rsh} check`
* Use `{!rsh} parallelReduce` for contracts that require loops and respond to `{!rsh} API`s on demand
* When waiting for a single `{!rsh} API` call without looping, use simpler tools such as `{!rsh} fork` or `{!rsh} call`, instead of a `{!rsh} parallelReduce`
* `{!rsh} 'use strict'` is ok, but not necessary. 
Use it only if you want.
* `{!rsh} verifyArithmetic` is good, but not necessary. 
Use it if you can. 
* `Refine` is a good way to ensure that invariants get checked at boundaries like `{!rsh} interact` and `{!rsh} publish`. 
Use it if applicable.

### Observing Time

Observing so called, "real" time with `{!rsh} absoluteSecs` and `{!rsh} relativeSecs` should be avoided. 
The aforementioned methods measure time passed on the blockchain. 
In the event of a network failure time will appear to stop when measured with `{!rsh} absoluteSecs` and `{!rsh} relativeSecs`. 
Contracts based on block number is a better option, however, this option can become unreliable if transaction speeds change over time. 
Consider your business and consumer needs when considering measuring the passage of time.

## Committing Artifacts

### Artifacts for the UI & Deployer

The output of the compiler can be committed so that UI developers don't need to run the compiler:

* Use the designated Reach version to compile: `REACH_VERSION=x.x reach compile`
* Commit `build/index.main.mjs` and any other files in `build/` that are needed
* Commit any additional artifacts, such as those useful for auditors
* gitignore everything else in `build/`

### Artifacts for Auditors

Committing intermediate artifacts can be useful for auditors. 
When preparing for an audit, compile with the `--intermediate-files` flag to persist them to your file system. 

For example, in an Algorand contract, `.dot` and `.teal` files may be helpful:
* `build/index.main.appApproval.teal`
* `build/index.main.state.dot`
* `build/index.main.appApproval.dot`

You can also generate `.png` images by using the `dot` program:
```
dot -Tpng -O build/index.main.state.dot > build/index.main.state.dot.png
```

## Testing

Write tests at the Reach level of abstraction without regard to the intended UI in which the Reach program will be used. 
The Reach contract should incorporate a robust test suite to ensure its security. Follow these guidelines when writing Reach tests:

* Remember that people and bots can send transactions directly to a contract without using a UI
* Tests should be runnable with `{!cmd} reach run`
  * Prefer to write the test suite in `index.mjs`
  * Prefer to use `{!js} test.one` and `{!js} test.run` from stdlib
* Tests should pass on all connectors (ETH, ALGO) that will actually be used by the DApp
* Tests should be plugged into CI and run on every PR that changes the contract
  * Tests should run using a designated version of Reach
  * It is useful to run a nightly job on the CI to detect regressions early
  * Tests should be included in build processes
  * PRs should not be merged until the CI passes correctly
* Tests should be self-contained and avoid relying on outside setup
* Tests should not rely on a fresh devnet, being the only thing running on devnet, and should avoid checking for exact block numbers
  * Sometimes this is unavoidable when the contract has differing behavior based on entering new states or ending based on elapsed block time
* Tests should demonstrate that the happy path leads to the desired outcomes
* Tests should demonstrate that unauthorized and invalid API calls are rejected by the contract
* It should be clear when and if tests are complete and no contract should be shipped without complete tests

## Integrate the Smart Contract

Integrate the smart contract into the UI after Reach tests pass:

* Use the project's specified version of `'@reach-sh/stdlib'`
* Retrieve the aforementioned contract artifacts
  * The UI can be created before the contract is finished, but it is critical to predetermine the `{!rsh} API`s, `{!rsh} View`s, and `{!rsh} Events`
* **Never use `Number`** when dealing with amounts of currency, especially fractional amounts. 
`Number` uses a floating point representation that is prone to weird rounding issues.
  * Use `BigInt` or `BigNumber` to represent the amount as an integer in its atomic unit
  * Use `n.toString()` or `JSON.stringify(n)` if it has to be formatted as JSON
  * Render to a user-friendly string at the last moment before displaying to the user
  * Parse and store as a `BigInt` or `BigNumber` as soon as possible when receiving input from a user

## Automate Testing

Test the DApp's UI as functionality is integrated. 
UI development is a granular process, in that each element builds upon the other:

* Test the UI with a devnet using `{!cmd} reach devnet`
* Run a devnet in the background with `{!cmd} reach devnet --await background`
* Prefer to test with devnet because it is much faster than testnet and can easily fund test accounts
* Engineers at Reach recommend using [cypress](https://www.cypress.io/) for automated UI testing
* Integrate automated UI testing into CI

## User Testing

Thorough user testing of the UI will eliminate embarrassing bugs and glitches in the user experience:

* Document a manual "smoke test" that is performed by individual developer team members prior to asking others to run user tests
  * Automate the smoke test to whatever degree possible and roll that work into the automated UI tests
  * Document remaining manual tests to quickly reproduce errors
* Work with a product manager to plan, announce, and release public test versions of the UI and associated smart contracts. 
Use the opportunity for PR to create buzz about the product. Proper timing for releases and promotions can be important.
* Prefer internal user testing before public user testing
* Incentivize public user testing to increase participation. A product manager can determine the proper incentives to offer.
* Apply a password to the UI for internal `testnet` testing
* Prefer testing with `testnet` rather than `mainnet` unless there's a compelling counter argument
* Perform a release to staging aimed at `mainnet` and user test before pushing to production

Slow is smooth. Smooth is fast. 
Taking time to identify the key goals and requirements of the DApp will inform what to test for and create an efficient development experience of the back and frontends. 
In turn, auditors will be able to more easily understand a highly structured application which will allow you to move to user testing and production in faster time. 
Join our [Discord](@{DISCORD}) for access to personalized help and guidance.

## Audits

Commission an audit of the smart contracts to provide peace of mind to your stakeholders. 
Reach has used FYEO and EKA Silicon for audits. 
They are prepared to audit Reach contracts and are able to provide such services for a lower fee and at a faster turn around time then other low-level language audits. 

Ensure the highest confidence in the contract before securing an audit.
Ideally, this confidence should be reinforced with successful UI tests before an audit is secured.
Reach is happy to offer code reviews and advice as part of our [in]Reach Partner Program.

## Launch

Following the best practices in this guide will result in faster DApp development cycles with fewer surprises. 
Encouraging test-driven development creates fewer vulnerabilities and provides the opportunity to create greater public confidence in your DApp. 
Our partners have realized a savings of 7x as compared to traditional blockchain development practices. 
We're happy to talk to you about your DApp and how you can use Reach to get to market safer and faster. 

Say [hello](https://share.hsforms.com/1EBAHnyBATkuxwTWFuV7UHgbm2jc) if you'd like to talk to Reach about our [in]Reach Partner Program.
