# {#guide-build-dapp} Building a Reach DApp

This guide is a high-level description of the process of building a Reach DApp.
It explains:

* Brainstorming DApp ideas
* The pre-coding process
* Scaffolding a DApp
* Participants
* Making incremental changes

## Brainstorming

If you do not know what DApp you want to build, then begin in the brainstorming stage.
Write issues that people complain about, things that bother you, and problems you see in your community, then write possible ways to solve the issue.
Make sure to be as specific as possible, because DApps with clear and concise goals tend to be stronger and are more likely to be completed.
For instance, maybe you often read headlines that online poker tournaments keep getting hacked, so you decide that the best way to fix this problem is to create a safe and secure blockchain DApp for playing poker by building with Reach.
This provides entertainment for people wanting to play online poker, but keeps their assets secure.

## Defining a DApp

Great, now you have a problem to solve, but where do you begin?

Before you start to code, a best-practice is to define the objectives, requirements, and a minimum viable product (MVP).
The objectives are the goals to achieve with your DApp to solve the problem.
Objectives should be measureable and specific and focus on the solution.
In our poker solution, an objective might be "have 100 daily users playing poker securely".

The requirements are the functionalities for the DApp, which include user login, design features, settings, etc.
One reason is that it is easier to make incremental changes and test to verify the results than it is to try to code everything at once.
This is especially true with large projects containing many functions.

A good MVP is going to be very narrow and only have necessary functionality, then functionality can be added bit by bit to make the final product.

For our MVP, we want users to get cards, place a bet, exchange cards, place a bet again, and then see who won.
 
The more narrow and concrete the requirements, the easier the coding will be, because narrow requirements provide clarity for how the code should be written.

After the requirements and assumptions are defined, write tests that will verify the code meets the requirements.
This is an often-neglected step, but can save time and effort in the long run.

Other items might come up as you are coding that you have not considered (such as someone going all-in), and that is okay.
Non-critical functionality do not need to be included in the MVP.
If you thing of something extra, make a note but don't include it in your MVP.
The purpose of this exercise is to organize how the DApp will be written, to prevent mistakes, and to reduce the probability of missing something important.

## Identify Participants

Now that you have defined the DApp and written unit tests, you can start creating your code.
A great place to begin is defining the `{!rsh} Participant`s in the `index.rsh` file.
`{!rsh} Participant`s are anything that will interact with your code, whether it is people or bots.
Each `{!rsh} Participant` type has an interact interface and this interact interface dictates how the user interacts with other participants and the contract.
For instance, a user in a poker game will decline or accept a wager, request cards, and fold.
These abilities are functions inside the interact interface for that user.
The first `{!rsh} Participant` deploys the DApp.
Other `{!rsh} Participant`s attach to the DApp.

If you will have multiple people choosing from the same set of actions, you should use `{!rsh} API` instead of using `{!rsh} Participant`.
An `{!rsh} API` is a set of users that all begin with the same options, but can make different choices.
Since the users playing poker are going to make different choices, we should make them an `{!rsh} API`.

An `{!rsh} Object` can also have an interact interface, .
This defines how a `{!rsh} Participant` interacts with the `{!rsh} Object`.
The deck of cards could be an example of an `{!rsh} Object` and the interact interface would be requesting cards.
We will want an `{!rsh} Object` for our card deck, but since most of the deck interactions are not in our MVP, we will skip the rest for now.

Now we understand all of the different interact interfaces, we can see we need a `{!rsh} Participant` for the deployer, an `{!rsh} API` for the players, and an `{!rsh} Object` for a card deck.
We need the `{!rsh} Participant` to have a method to start the game and one to see the outcome.
The `{!rsh} Object` needs to have a deck of 52 cards in 4 suits with defined attributes (such as number value and suit).
The `{!rsh} API` needs to have a method to accept the hand and see the outcome.

Once the `{!rsh} Participant`s, `{!rsh} Object`s, and `{!rsh} API`s are defined in the `index.rsh` file, you should recreate the same objects and methods as interact objects in the `index.mjs` frontend file.

## Writing basic functionality

After writing the tests and creating the interact interface for users, we need to define the basic functions of the app.
These are the functions that are required before everything else can be added.
For our basic functionality, we need:

* Addresses for the users so they can attach to the contract
* Funds to be used for attaching to the contract and betting later
* A contract that defines the bet
* To deal each user five (5) cards
* To determine which hand is the winner

First, you want to create a wallet for each user.
Then, the wallet can be funded with fake tokens for testing.
These wallets and funds are supplied by users via a wallet connection in Mainnet DApps, but we don't want to connect to the Mainnet until we are ready to deploy our DApp.
You can then define a contract where each user accepts an initial wager to start the game.
Once the DApp is ready for deployment, we will swap out the test accounts for a wallet connection.

## Incremental changes

After you have coded the basic structure and it is working, the next thing to do is add incremental changes to add functionality.
For instance, for your Reach blockchain version of poker, you would need to define:

* Placing bets
* Raising bets
* Calling bets
* Folding
* A way to trade cards for different ones

Write a test for the functionality, and then write the function and see if the test passes.
This makes sure that there are no issues with that individual function.
When that function works, add another function and test again.
Continue this process until all of the functions of the MVP are complete.

Join our [Discord community](@{DISCORD}) or checkout [GitHub Discussions](https://github.com/reach-sh/reach-lang/discussions) for more assistance with building DApps in Reach.
