


# {#guide-deploymode} Choosing a deployment mode

`Reach.App` takes an option named `deployMode` (as described [in the reference](##ref-programs-reach.app)) that determines how the contract of a Reach application should be deployed.

The options are:

**Constructor (`'constructor'`).**
In this mode, the contract is deployed independently of all of the participants and available to the public to connect to.
This is the most flexible option, because it imposes no restrictions on the body of the application.
It has the downside of being slightly more expensive, because the construction of the contract must be paid for independently of the first use of the contract.
Furthermore, the creation of the contract is public, so it is possible for an agent `X` to create a contract `F` and intend to play the role of participant `A` in it, but the creation of the contract is observed by a third-party, `Z`, who interacts with `F` before `X` and acts as `A`.
This is particularly nefarious if `X` has already shared the information about the contract with their intended counter-party, `Y`.

**Delayed (`'firstMsg'`).**
In this mode, the contract is deployed at the time of the first publication in the application.
This is slightly more efficient, because the contract creation is bundled with the first application.
This means that the contract does not exist until after the application starts running, which means that some features, like `wait` and `.timeout`, as well as collective operations, are not available until after the first action.
Furthermore, it complicates the sharing of information about a contract by the deployer with a third-party, because the deployer must introduce an `interact` method to transfer control to the `frontend` which will call (e.g.) `await ctc.getInfo()` and extract the information about the program.
If the frontend attempts to call `await ctc.getInfo()` too early, it will [deadlock](https://en.wikipedia.org/wiki/Deadlock).

For example, consider the frontend in the seventh version of the _Rock, Paper, Scissors!_ tutorial:

@[code{23-33}](@reach-lang/examples/tut-8/index.mjs)

+ Line 26 instructs the backend that it is the deployer.
+ Line 27 immediately reads the contract information.


If [tut-8/index.rsh](@github/examples/tut-8/index.rsh) were defined to use `deployMode` `'firstMsg'`, then this call would [deadlock](https://en.wikipedia.org/wiki/Deadlock), because the contract information would not yet be available.
(Furthermore, the whole premise of this code, where on line 24, the user is asked if they will deploy, is unnecessary, because Alice would _always_ deploy.
The code for the non-deploy case, line 33, would move to exclusively occur on Bob's branch.)
Instead, the Reach application would need to introduce a new `interact` method called by Alice _after_ the first message to observe the contract information.

@[code{42-48}](@reach-lang/examples/tut-8/index.rsh)

This method would be called on line 47 in a new `only` block.

This complexity is why `'firstMsg'` is not the default, despite being more efficient.

**Factory (`'factory'`).**
A future version of Reach will support a "factory" deployment mode where the contract is independently deployed into a meta-contract with a single method that creates new instances of its child contract.
This style of deployment has the same restrictions as the (`'firstMsg'`) style, but can be even more efficient if a contract family is used many times.
Furthermore, the contract factory may emit events that can be used to more easily observe the creation of new contract (i.e. application) instances.
