# {#guide-build-dapp} Building a Reach DApp

This guide is a high-level description of the process of building a Reach DApp.
It explains:

* Brainstorming
* Definitions
* Participants
* Incremental Changes

## Brainstorming

If you do not know what DApp you want to build, then begin in the brainstorming stage.
Write issues that people complain about, things that bother you, and problems you see in your community.
Then write possible ways to solve the issue.
Make sure to be as specific as possible, because DApps with clear and concise goals tend to be stronger and are more likely to be completed.
For instance, maybe your friends frequently complain about the high fees of NFT marketplaces, so you decide to create one with low fees.

## Defining a DApp

Great, now you have a problem to solve, but where do you begin?

Before you start to code, a best practice is to define the objectives, requirements, and a minimum viable product (MVP).
The requirements are the goals to achieve with your DApp to solve the problem.
Requirements should be measureable and specific and focus on the solution.
In our poker solution, requirements might be "put an NFT up for sale based on the NFT ID" or "have users buy and sell NFTs".

The requirements are the functionalities for the DApp, which include wallet connection, design features, settings, etc.
One reason is that it is easier to make incremental changes and test to verify the results than it is to try to code everything at once.
This is especially true with large projects containing many functions.

A good MVP is going to be very narrow and only have necessary functionality, then functionality can be added bit by bit to make the final product.

For our MVP, we want to create an NFT, put it up for sale, and have a user buy it.
 
The more narrow and concrete the requirements, the easier the coding will be, because narrow requirements provide clarity for how the code should be written.

After the requirements and assumptions are defined, write tests that will verify the code meets the requirements.
This is an often-neglected step, but can save time and effort in the long run.

Other items might come up as you are coding that you have not considered and that is okay.
Non-critical functionality do not need to be included in the MVP.
If you think of something extra, add it to your feature backlog, but don't include it in your MVP.
The purpose of this exercise is to organize how the DApp will be written, to prevent mistakes, and to reduce the probability of missing something important.

## Identify Participants

Now that you have defined the DApp and written unit tests, you can start creating your code.
A great place to begin is defining the `{!rsh} Participant`s in the `index.rsh` file.
`{!rsh} Participant`s are anything that will interact with your code, whether it is people or bots.
Each `{!rsh} Participant` type has an interact interface and this interact interface dictates how the user interacts with other participants and the contract.
For instance, the creator of the NFT will need to be able to get the NFT ID.
These abilities are functions inside the interact interface for that user.
The first `{!rsh} Participant` deploys the DApp.
Other `{!rsh} Participant`s attach to the DApp.
Our deployer `{!rsh} Participant` might look something like this:

```rsh
const Creator = Participant('Creator', {
  getId: Fun([], UInt) });
```

If you will have multiple people choosing from the same set of actions, you should use `{!rsh} API` instead of `{!rsh} Participant`.
An `{!rsh} API` is a set of users that all begin with the same options, but can make different choices.
Since the users are all buying NFTs, we should make them an `{!rsh} API`.

```rsh
const Owner = API('Owner', {
  newOwner: Fun([], Address),
  showOwner: Fun([UInt, Address], Null) });
```

An `{!rsh} Object` can also have an interact interface.
This defines how a `{!rsh} Participant` interacts with the `{!rsh} Object`.
Our MVP does not require an `{!rsh} Object` with an interact interface.

Now we understand all of the different interact interfaces, we can see we need a `{!rsh} Participant` for the deployer and an `{!rsh} API` for the buyers.

The `{!rsh} API` needs to have a method to accept the hand and see the outcome.

Once the `{!rsh} Participant`s, `{!rsh} Object`s, and `{!rsh} API`s are defined in the `index.rsh` file, you should recreate the same objects and methods as interact objects in the `index.mjs` frontend file.

```rsh
const makeOwner = (acc, who) => {
  const ctc = acc.contract(backend, ctcAlice.getInfo());
  const others = everyone.filter(x => x[0] !== who);
  return backend.Owner(ctc, {
    newOwner: (async () => {
      await externalViewer();
      if ( trades == 0 ) {
        console.log(`${who} stops`);
        process.exit(0);
      }
      const next = randomArrayRef(others);
      console.log(`${who} trades to ${next[0]}`);
      trades--;
      return next[1];
    }),
    showOwner: ((id, owner) => {
      if ( stdlib.addressEq(owner, acc) ) {
        console.log(`${who} sees that they own it`);
      } else {
        console.log(`${who} sees that ${stdlib.formatAddress(owner)} owns it`);
      }
    }),
  });
};
```

## Writing basic functionality

After writing the tests and creating the interact interface for users, we need to define the basic functions of the app.
These are the functions that are required before everything else can be added.
For our basic functionality, we need:

* Addresses for the users so they can attach to the contract
* Funds to be used for attaching to the contract and betting later
* A contract that defines the bet
* To deal each user five (5) cards
* To determine which hand is the winner

First, you want to create test funds.
This will be used to fund each test wallet for the users later.

```js
const startingBalance = stdlib.parseCurrency(100);
```

Now you can create test wallets and fund them.

```js
const buyers = await stdlib.newTestAccount(startingBalance);
```

These wallets and funds are supplied by users via a wallet connection in Mainnet DApps, but we don't want to connect to the Mainnet until we are ready to deploy our DApp.
Once the DApp is ready for deployment, we will swap out the test accounts for a wallet connection.

## Incremental changes

After you have coded the basic structure and it is working, the next thing to do is add incremental changes to add functionality.
For instance, for the NFT marketplace, you would need to define:

* Auctions
* Deadlines
* Countdown timers
* Royalties
* Trading NFTs

Write a test for the functionality, and then write the function and see if the test passes.
This makes sure that there are no issues with that individual function.
When that function works, add another function and test again.
Continue this process until all of the functions of the MVP are complete.

Join our [Discord community](@{DISCORD}) or checkout [GitHub Discussions](https://github.com/reach-sh/reach-lang/discussions) for more assistance with building DApps in Reach.
