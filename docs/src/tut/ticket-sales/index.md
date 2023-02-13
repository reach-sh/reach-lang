# {#ticket-sales} Ticket Sales

This tutorial will demonstrate a token distribution mechanism in Reach. 

This token distribution DApp could be used for many different applications, but we thought it would be best used to demonstrate a decentralized DApp replacement for Ticketmaster.

It assumes prior knowledge of Reach: we recommend completing the [Rock, Paper, Scissors](##tut) tutorial first.

And maybe [RSVP](##tut-rsvp). 

And maybe [Wisdom For Sale](##wfs). 

And maybe [ERC20](##erc20). 

Just do them all, you will be really good after.

We assume you are working in a project folder called `ticket-sales`:
```cmd
$ mkdir ticket-sales
$ cd ticket-sales
```

Start by creating your files:
```cmd
$ touch index.rsh index.mjs
```
Our application is going to allow the Administrator to provide details about the token sale and then allow users to come in and buy tokens until the contract balance is zero. 

At that time it will close the sale functions and transfer the total network tokens collected back to the Administrator.

As with any Reach DApp, it is best to first think about who the users are in our application.

There will be one Deployer (the Admin) providing the parameters of the sale, including the non-network tokens (or tickets).
```
load: /examples/ticket-sales/index.rsh
md5: c425745032273893d106fe3de005f15e
range: 1-11
```
- Line 4 declares a single `{!rsh} Participant` to bind as the `Admin`.
- Lines 5-8 declares the parameters to be passed to the contract initially.
- Line 10 declares a `launched` function, common in this style of application. [Why?](##erc20)


We then need a dynamic amount of users whose functionality will be repeated.
```
load: /examples/ticket-sales/index.rsh
md5: c425745032273893d106fe3de005f15e
range: 12-15
```
- Line 12 defines a dynamic amount of users, all with shared abilities.
- Line 13 declares a `buyTicket` function that our Buyer(s) will be able to call.

Now we've defined our users and the functions they will be allowed, we call `{!rsh} init()` to start stepping through the states of our program.

Now the Admin can actually provide the values for the parameters we've declared. This happens in an Local Step. 

[Read more about Modes of a Reach App](https://github.com/reach-sh/reach-lang/discussions/1171).
```
load: /examples/ticket-sales/index.rsh
md5: c425745032273893d106fe3de005f15e
range: 17-21
```
- Line 17 starts the Local Step.
- Line 18 declassifies the parameters and unpacks them into respective constants.
- Line 20 publishes those values to the blockchain.
- Line 21 `{!rsh} commit`s to move us out of consensus step.

Now, the next step is to have the Admin actually pay the non-network tokens into the contract. This needs to happen in a consensus step.

Why then did we move *out* of consensus on Line 21?

The answer is that we need to `{!rsh} publish` the non-network token `tok` before we can `{!rsh} pay` it into the contract. 

Reach supports network tokens by default, but a non-network token, like the one in our program, needs to be taught to Reach before you can pay it in.

The general flow for paying non-network tokens:
1. Get token info in Local Step.
2. `{!rsh} publish` token info in Consensus Step.
3. `{!rsh} commit()`.
4. `{!rsh} pay` tokens in Consensus Step.

That means our next step is to `{!rsh} pay` the tokens into the contract.
```
load: /examples/ticket-sales/index.rsh
md5: c425745032273893d106fe3de005f15e
range: 22-23
```
- Line 22 pays the `supply` of `tok` from `A` into the contract account. This must be a syntactic tuple where the general structure is `{!rsh} A.pay([networkTokenAmount, [amount, non-networkTokens]]);`.
- Line 23 notifies the frontend that our contract is ready to start accepting API calls, this can only be called after the first `{!rsh} publish`.

This is a good place to pause in our Reach `rsh` file and get into the frontend `mjs` testing file.

Let's start with some basics.
```
load: /examples/ticket-sales/index.mjs
md5: d2b60e84f70854bc18c26764bdcaf1aa
range: 1-9
```
- Line 1-2 are necessary imports.
- Line 3 sets a constant for the standard library and hides warning messages.
- Line 4 sets a constant for the `MAX` number of tickets. Changing this number does so across the entire program.
- Line 7 launches a new token with a `MAX` supply. Alternatively, you could replace `tickets.id` with a known testnet token id string.
- Line 9 welcomes you to the ticket sales revolution. Just make sure you [print enough pamphlets](https://youtu.be/Kb1ztV93dsE).

:::note
`REACH_NO_WARN: 'Y'`

It is *very* important for you as a programmer to understand the warning messages Reach is giving you before you silence them. Many times, these warnings are indicative of critical DApp design flaws.

If you remove this from this particular program, Reach will warn you that you are using `stdlib.newTestAccount` and this cannot be used outside of devnet environments. When you move to TestNet, you'll need to get the users account via `stdlib.getDefaultAccount()`.
:::

Now we'll fast forward to the end of our test suite and add the functionality for our `Admin`.
```
load: /examples/ticket-sales/index.mjs
md5: d2b60e84f70854bc18c26764bdcaf1aa
range: 24-35
```
- Line 24 creates a promise with the `Admin` contract handle.
- Lines 25-28 specify the parameters of our sale.
- Line 30 defines our `launched` function, which starts the chain of API calls to our contract with `startBuyers()`.
- Line 35 is useful to note our test suite reached its exit normally.

Our frontend test suite `mjs` is now caught up to our Reach `rsh` file.

Let's move back to the Reach file and actually create our API function.

This program ends up being short and compact because of the `{!rsh} parallelReduce` control structure that we'll implement to allow access to our API functions in a looping construct.

Read all about it in our @{seclink("guide-parallelReduce")}.

In this case, we want the loop to allow the function to be callable until the contract is out of tokens. This is an important piece of information for our design.
```
load: /examples/ticket-sales/index.rsh
md5: c425745032273893d106fe3de005f15e
range: 25-28
```
- Line 25 declares a new `{!rsh} parallelReduce` and sets it up to track a single value `ticketsSold`, which is initialized to zero.
- Line 26 states an `{!rsh} invariant` about our loop that the network token balance will always equal `cost * ticketsSold`.
- Line 27 states an `{!rsh} invariant` about our loop that the non-network token balance will always equal the initial `supply - ticketsSold`.
- Line 28 sets our `{!rsh} while` loop to run until `ticketsSold == supply` at which point it will exit the `{!rsh} parallelReduce`.

It is important to note the relationship between the values above. 

The Reach compiler will check your DApp for *all* of the possibilities related to your program values in an *unbounded* way, bound only by the limits of the data type you are working with. 

Reach will put bounds on those checks as determined by your verification checks and `{!rsh} invariant`s.

By tightly tracking all of the `{!rsh} invariant` values in our `{!rsh} parallelReduce` LHS and `{!rsh} while` loop -- and relating them to the contract balance of network tokens and non-network tokens, we can satisfy the Reach Verification Engine to produce no errors.

Read more about @{seclink("guide-assert")}.

Understanding this relationship is key to building Reach DApps.

Now we can actually define our API function.
```
load: /examples/ticket-sales/index.rsh
md5: c425745032273893d106fe3de005f15e
range: 29-36
```
- Line 29 defines an API macro `{!rsh} .api_` from the `B` API with the name `buyTicket`. It takes no parameters.
- Line 30 starts the outer `{!rsh} return`. Here we sepecify a `PAY_EXPR` of `cost` and declare our `{!rsh} return` function `ret`.
- Line 31 sends one non-network token (ticket) to the API caller.
- Line 32 returns `{!rsh} null` to the caller.
- Line 33 is our inner `{!rsh} return`, where we increment `{!rsh} parallelReduce` LHS value `ticketsSold`.

:::note
On incrementing, Reach *does not* support incrementing with `i++`, because this is a mutation of `i` and Reach insists that we always return new, updated values.

Use `i + 1` instead.
:::

That is all for our API member function `buyTicket`. 

After this loop terminates, the next step is to transfer the network token balance to the ticket seller.
```
load: /examples/ticket-sales/index.rsh
md5: c425745032273893d106fe3de005f15e
range: 37-39
```
- Line 37 transfers the exact known balance back to the seller. It is bad practice to use `{!rsh} transfer(balance()).to(A)` here. Instead, use a formula that evaluates to the known balance.
- Line 38-39 has us `{!rsh} commit` and `{!rsh} exit` the DApp.

We are almost done. The final step is to test our API function by making some calls in the frontend `mjs` file.

To test our users, we need functions to create accounts and contract information.
```
load: /examples/ticket-sales/index.mjs
md5: d2b60e84f70854bc18c26764bdcaf1aa
range: 11-22
```
- Line 11 is the outer function that will be called from `launched` to start making API calls.
- Line 12 will allow for repeating actions for individual users.
- Line 13-14 creates an account, funds it, and connects it to the Admins contract instance.
- Line 15 has the account opt-in to our ticket token.
- Line 16 is our API call. It uses the contract handle to access the `Buyer.buyTicket` function.
- Line 19-20 is a looping construct for easily repeating users up to `MAX`.

That is everything you need for a decentralized [ticket-sales](https://github.com/reach-sh/reach-lang/tree/master/examples/ticket-sales) contract in Reach. 

Let's run it..
```cmd
$ ./reach run
```

:::note
If you have not specified a network via `reach config`, you will need to specify one with `REACH_CONNECTOR_MODE`.
:::

The output looks something like:
```
Welcome to the ticket distributor
Lets get you a ticket
Ready at contract: 407
Tickets sold: 0
Tickets sold: 1
Tickets sold: 2
Tickets sold: 3
Tickets sold: 4
Tickets sold: 5
Tickets sold: 6
Tickets sold: 7
Tickets sold: 8
Tickets sold: 9
Tickets sold: 10
Tickets sold: 11
Tickets sold: 12
Tickets sold: 13
Tickets sold: 14
Tickets sold: 15
Tickets sold: 16
Tickets sold: 17
Tickets sold: 18
Tickets sold: 19
Tickets sold: 20
Tickets sold: 21
Tickets sold: 22
Tickets sold: 23
Tickets sold: 24
Tickets sold: 25
Tickets sold: 26
Tickets sold: 27
Tickets sold: 28
Tickets sold: 29
Tickets sold: 30
Tickets sold: 31
Tickets sold: 32
Tickets sold: 33
Tickets sold: 34
Tickets sold: 35
Tickets sold: 36
Tickets sold: 37
Tickets sold: 38
Tickets sold: 39
Tickets sold: 40
Tickets sold: 41
Tickets sold: 42
Tickets sold: 43
Tickets sold: 44
Tickets sold: 45
Tickets sold: 46
Tickets sold: 47
Tickets sold: 48
Tickets sold: 49
Exiting...
```
Again, the number of tickets sold here can be changed by editing the `MAX` constant.

Next, we'll start working on a frontend to demonstrate integrating this Reach DApp with React.

This code is open source. Nudge, nudge, wink, wink.

Someone use it to unseat Ticketmaster, please. 

We are tired of paying $3k USD for Taylor Swift tickets.
