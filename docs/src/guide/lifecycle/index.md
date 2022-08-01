# {#guide-lifecycle} Contract Development Lifecycle

Smart contracts impose unique restrictions on development that is not experienced in web2. 
Building web3 DApps with intentionality enables efficient use of a developer's time and reduces exposure to unforeseen vulnerabilities. 

## Key Ideas and Goals

Answer the following questions to better understand the problem your DApp will solve:

* What specific problem will you solve?
* What does the DApp need to accomplish?
* Who is it trying to impress and why?
* Will incentives algin for people to use the DApp in the intended manner?
* What is the minimum viable product (MVP)?
* Is there a revenue model? Is it fair? Will users agree?
* Is decentralization adding real value for users? Could it be on a database instead of blockchain?
* Can it be more decentralized to add even more value for users? Is there adequate incentive (profit, reputation, other) to do it that way?
* Create answers that are as specific as possible. DApps with clear objectives are more likely to succeed

## Requirements and Limitations

Determine the requirements and limitations of the smart contract. 
Requirements should be measurable, specific, and solution-oriented. 
The initial product should be narrow in scope; fight scope-creep by storing new feature suggestions in a backlog.

Once you understand the purpose of the DApp, you can clarify who will interact with the DApp and what restrictions each participant will experience. 
This can be thought of as the communication model of the application. 

* Describe the participants (human or bot)
  * How they will interact with the contract
  * What will they cause the contract to do?
  * What information will the contract return?

For example, participants may include:

+ A Deployer
+ One or more Admins
  * Is the Deployer the only Admin? 
  * Can Admins change over time?
+ `{!rsh} API` participants who add resources to the contract
+ `{!rsh} API`s with special access to extract resources from the contract
  * What must be true to enable this functionality?
+ `{!rsh} API`s who interact with the contract in a completely unprivileged manner

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

Properly prepared requirements should be encapsulated within Reach `{!rsh} API`s, `{!rsh} View`s, `{!rsh} Events`, and `{!rsh} Participant`s. 
Accurate descriptions will allow the smart contract and the UI to be developed simultaneously, by coding against the same expectations.

## Developing the Smart Contract

Now that the communication model of the application is clearly understood, you are able to codify the `{!rsh} API`s, `{!rsh} View`s, `{!rsh} Events`, and `{!rsh} Participant`s into the App Initialization mode. 
The `{!rsh} interact` interface dictates how users interact with the contract and other `{!rsh} Participant`s. 
The App Initialization mode should be written to be platform-agnostic. 
Any platform-specific elements should be implemented via the appropriate `{!rsh} interact` interface. 

The following best practices in contract design create Reach DApps that are efficient and faster to debug:
* Prefer to name the Reach file `index.rsh`
* In most cases, prefer `{!rsh} API`s over `{!rsh} Participant`s
* Always include `{!rsh} invariant`s about the contract balance(s) for all currencies in a `{!rsh} while` loop
* Every time a theorem fails within a `{!rsh} while` loop your first impulse should be to add an `{!rsh} invariant`, not a `{!rsh} check`
* Use `{!rsh} parallelReduce` for contracts that require loops and respond to `{!rsh} API`s on demand
* When waiting for a single `{!rsh} API` call without looping use simpler tools such as `{!rsh} fork` or `{!rsh} call` instead of a `{!rsh} parallelReduce`
* `{!rsh} 'use strict'` is ok, but not necessary. 
Use it only if you want.
* `{!rsh} verifyArithmetic` is good, but not necessary. 
Use it if you can. 
* `Refine` is a good way to ensure that invariants get checked at boundaries like `{!rsh} interact` and `{!rsh} publish`. 
Use it if applicable.

### Observing Time

Observing so called, "real time" with `{!rsh} absoluteSecs` and `{!rsh} relativeSecs` should be avoided. 
The aforementioned methods measure time passed on the blockchain. 
In the event of a network failure time will appear to stop when measured with `{!rsh} absoluteSecs` and `{!rsh} relativeSecs`. 
Contracts based on block number is a better option, however, this option can become unreliable if transaction speeds change over time. 
Consider your business and consumer needs when considering measuring the passage of time.

## Committing Artifacts

The output of the compiler can be committed so that UI developers don't need to run the compiler:

* Use the designated Reach version to compile: `REACH_VERSION=x.x reach compile`
* Commit `build/index.main.mjs` and any other files in `build/` that are needed
* Commit any additional artifacts, such as audits
* gitignore everything else in `build/`

## Testing

Write tests at the Reach level of abstraction without regard to the intended UI in which the Reach program will be used. 
The Reach contract should incorporate a robust test suite to ensure its security. Follow these guidelines when writing Reach tests:

* Remember that people and bots can send transactions directly to a contract without using a UI
* Tests should be runnable with `{!cmd} reach run`
* Prefer to write the test suite in `index.mjs`
* Prefer to use `{!js} test.one` and `{!js} test.run` from stdlib
* Tests should pass on all connectors (ETH, ALGO, CFX) that will actually be used by the DApp
* Tests should be plugged into CI and run on every PR that changes the contract
* Tests should run using a designated version of Reach
* It is useful to run a nightly job on the CI again Reach `master` version to detect regressions early
* Tests should be included into build processes
* Tests should be self-contained and avoid relying on outside setup
* Test should not rely on a fresh devnet, being the only thing running on devnet, and avoid checking for exact block numbers
  * Sometimes this is unavoidable when the contract has differing behavior based on entering new states or ending based on elapsed block time
* Tests should demonstrate that the happy path leads to the desired outcomes
* Tests should demonstrate that unauthorized and invalid API calls are rejected by the contract

## Integrate the Smart Contract

Integrate the smart contract into the UI after Reach tests pass:

* Use the project's specified version of `'@reach-sh/stdlib'`
* Retrieve the aforementioned contract artifacts
  * The UI can be created before the contract is finished, but it is critical to predetermine the `{!rsh} API`s, `{!rsh} View`s, and `{!rsh} Events`
* **Never use `Number`** when dealing with amounts of currency, especially fractional amounts. `Number` uses a floating point representation that is prone to weird rounding issues.
  * Use `BigInt` or `BigNumber` to represent the amount as an integer in its atomic unit
  * Use `n.toString()` or `JSON.stringify(n)` if it has to be formatted as JSON
  * Render to a user-friendly string at the last moment before displaying to the user
  * Parse and store as a `BigInt` or `BigNumber` as soon as possible when receiving input from a user

### Choosing the UI

You may choose to use any frontend language or framework to interact with the Reach smart contract. 
However, Reach provides the ability to quickly create React scaffolding with `{!cmd} reach react`

* If you do not have a frontend preference then we suggest using React with Reach

## Automate Testing

Test the DApp's UI as functionality is integrated. 
UI development is a granular process, in that each element builds upon the other:

* Test the UI with a devnet using `{!cmd} reach devnet`
* Run a devnet in the background with `{!cmd} reach devnet --await background`
* Prefer to test with devnet because it is much faster than testnet and can easily fund test accounts
* Engineers at Reach recommend using [cypress](https://www.cypress.io/) for automated UI testing
* Integrate automated UI testing into CI

## Audits

Commission an audit of the smart contracts to provide peace of mind to your stakeholders. 
Reach has used Kudelski and EKA Silicon for audits. 
They are prepared to audit Reach contracts and are able to provide such services for a lower fee and at a faster turn around time then other low-level language audits. 

Committing additional intermediate artifacts can be useful for auditors. When preparing for an audit, compile with the `--intermediate-files` flag to persist them to your file system. 

For example, in an Algorand contract, `.dot` and `.teal` files may be helpful:
* `build/index.main.appApproval.teal`
* `build/index.main.state.dot`
* `build/index.main.appApproval.dot`

You can also generate `.png` images by using the `dot` program:
```
dot -Tpng -O build/index.main.state.dot > build/index.main.state.dot.png
```

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