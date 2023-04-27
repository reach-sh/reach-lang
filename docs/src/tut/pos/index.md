# {#point-of-sale} Point-of-sale

This tutorial will walk you through creating a business point-of-sale machine, like Toast or Square.

It assumes that you are comfortable with writing simple Reach DApps and have already completed one or two of our [tutorials](##tuts). 

Complete them all to become a Reach rockstar.

## Program Design
The main function of our DApp will be to process sale transactions. This means we will need to implement functions for purchases and refunds. 

We will allow purchases of varying amounts for different products, so we will also need to track the amount of each users purchase.

It would also be good to reward our business' visitors with a Loyalty Token. Future programs could use this Loyalty Token to gate access to special features or events.

When designing our DApp we need to consider possibilities for the length of our contract. 

Given the Loyalty Token, it will be best for our implementation to make contract functions accessible until we run out of Loyalty Tokens.

### Here is the general flow of our DApp:
1. Admin gives parameters and deploys contract.
2. Admin deposits non-network Loyalty Tokens into contract.
3. Contract exposes API functions.
4. Users purchase items in our store.
5. Users are offered a refund window.
6. Close access to API functions when Loyalty Token balance is zero.
7. Transfer network tokens to Admin.
8. Terminate contract.

## Where the users at?
At this point of designing our Reach DApp, we need to ask ourselves how many and what types of users will be interacting with it.

We know that we need at least one `{!rsh} Participant` to deploy the contract and serve as our `Admin`. We can consider them our shop owner.

:::note
All Reach DApps require at least one `{!rsh} Participant`, because `{!rsh} API` members *do not* have the ability to deploy contracts.
:::

The rest of our users will be customers in our shop, so they are best represented as `{!rsh} API`. This will allow an unlimited number of unknown users to interact with our contract `{!rsh} API` member functions.

Let's write some Reach code for our users.
```
load: /examples/point-of-sale/index.rsh
md5:  2d9202924ee606ab47726038fbde05a3
range: 1-12
```
- Line 4 is a new feature with Reach 0.1.13, read more about `{!rsh} ALGOExitMode`.
- Line 5 declares our 'Admin' `{!rsh} Participant`.
- Line 6-9 declares data types of contract parameters.
- Line 11 is our signature `launched` function, to notify the frontend that we are ready to start accepting API calls.

Now for our `{!rsh} API` users.
```
load: /examples/point-of-sale/index.rsh
md5:  2d9202924ee606ab47726038fbde05a3
range: 13-17
```
- Line 13 specifies our `Buyer` as an `{!rsh} API`.
- Line 14-15 declare the functions that this type of user is allowed to access.
- Line 17 calls `init()` to start stepping through the states of our DApp.

Now that our users are defined, the first step from our General Flow is to get the parameters from the `Admin`.
```
load: /examples/point-of-sale/index.rsh
md5:  2d9202924ee606ab47726038fbde05a3
range: 19-25
```
- Line 19 starts a local step.
- Line 20 gets the parameters from the `Admin` frontend and unpacks them into respective constants.
- Line 22 `{!rsh} publish`es these identifiers to the blockchain.
- Line 23 has us `{!rsh} commit()` to move out of consensus. Now our contract is aware of `tok` and it can be paid into the contract.
- Line 24 `{!rsh} pay`s the total supply of Loyalty Tokens into the contract.
- Line 25 Notifies the frontend that our contract is ready to start accepting `{!rsh} API` calls.

The first two steps of our General Flow are now complete.

Now is a good time to get into the frontend `mjs` test suite and get that started.
```
load: /examples/point-of-sale/index.mjs
md5:  5229e4cebe07397fe5e73a60dc19f70f
range: 1-15
```
- Lines 8-9 create a new account for our `Admin` and get the contract handle.
- Line 10 launches our Loyalty Token, depositing the total supply to `accA`.
- Lines 12-15 is a welcome message that is probably too long.

Now let's automate our `Admin` interaction. Note the jump in line numbers, this code goes at the bottom of your `mjs` file.
```
load: /examples/point-of-sale/index.mjs
md5:  5229e4cebe07397fe5e73a60dc19f70f
range: 45-56
```
- Line 46-49 are various parameter values provided by the Admin.
- Line 51-53 is our function that will be called when the contract is ready to start accepting `{!rsh} API` calls. Note the `startBuyers()` function that we will implement later.
- Line 56 is useful to know that our test suite exited normally. It helps boost your moral.

Okay, now we are ready to get back to our General Flow list!

The next step is number 3, to expose `{!rsh} API` functions for our customers to call. This is a job for our favorite control structure `{!rsh} parallelReduce`. 

We'll start with setting up the structure.
```
load: /examples/point-of-sale/index.rsh
md5:  2d9202924ee606ab47726038fbde05a3
range: 27-33
```
- Line 27 defines a new `{!rsh} Map` for storing users and their purchase amounts.
- Line 28 starts our `{!rsh} parallelReduce` tracking two values, `tokensSold` and `total`. These values are initialized each to zero.
- Line 29 specifies a `{!rsh} paySpec`. This is used to allow the `{!rsh} parallelReduce` to *accept* non-network tokens as payment. Notice the [ticket-sales](##ticket-sales) example *does not* use `{!rsh} paySpec`, because it *pays out* non-network tokens, it **does not** *accept* payment in those same tokens.
- Line 30-32 set up `{!rsh} invariant`s for our loop. Each one of these is essential to compilation and the security of our program.
- Line 33 sets our loop condition. This will run until the contract is out of Loyalty Tokens.

Now that the looping construct is set up for *when* to expose these functions, we need to write the function definitions. 

We'll write each `purchase` and `refund` individually. First the `purchase` function.
```
load: /examples/point-of-sale/index.rsh
md5:  2d9202924ee606ab47726038fbde05a3
range: 34-45
```
- Line 34 is our function heading.
- Lines 35-37 are necessary dynamic checks to be executed when this function is called. These act as gates for this function and help the Reach Verification Engine better understand the bounds of our DApp.
- Line 38 sets up our outer `{!rsh} return`. This may be the first time you have seen this syntax. Recall, the first argument for the outer return is the `PAY_EXPR` -- but we specified a `{!rsh} paySpec([tok])` so we need to account for these tokens as well. The general syntax here is.. `{!rsh} return[[networkTokenAmount, [amount, nonNetworkTok]]`. That means that our function is specifically asking the user to pay `purchasePrice` in network tokens and `[0, tok]` of our Loyalty tokens.
- Line 39 adds the caller to the `{!rsh} Map` with their purchase amount as the value. This allows us to look up this value later should the user need a refund.
- Line 40 adds one to the `tokensSold` variable.
- Line 41 `{!rsh} transfer`s a single Loyalty Token to the purchaser.
- Line 42 returns our new variable to the caller.
- Line 43 updates the `{!rsh} parallelReduce` LHS values

That is all for our `purchase` function!

The next piece to add is the `refund` -- but hopefully for our Admin, nobody needs one. 

If we do process a refund, the DApp requires the purchaser to return their Loyalty Token -- you don't get to keep both.
```
load: /examples/point-of-sale/index.rsh
md5:  2d9202924ee606ab47726038fbde05a3
range: 46-55
```
- Line 46 starts our api macro `{!rsh} .api_`.
- Line 47 is a dynamic check to ensure that the caller is in our `{!rsh} Map`, signifying that they have made a purchase.
- Line 48 starts our outer `{!rsh} return` and is the real reason we needed `{!rsh} paySpec` syntax. Here we accept `[1, tok]` from the caller.
- Line 49 retrieves the users value from the `{!rsh} Map`. If it is not there, we default to 0. Read more about why we do this in our [ERC20 tutorial](##erc20).
- Line 50 transfers the users purchase amount from the contract account in network tokens to the caller.
- Line 51 returns the `{!rsh} Map` value to the caller.
- Line 52 deletes this user from the `{!rsh} Map`, so they cannot call `refund` again.
- Line 53 updates the `{!rsh} parallelReduce` LHS values.

Really the heart of our entire DApp is in these two functions.

In 22 lines of Reach code, we can process varying purchase amounts in network tokens, reward users with Loyalty Tokens, and even process refunds!

The next step in our General Flow is to assume that we close these functions because we are sold out of Loyalty Tokens. We will write this functionality into our frontend test suite later.

For now, let's finish up our Reach `rsh` code. All that is left is to `{!rsh} transfer` the network token balance to the `Admin` and `{!rsh} exit` the DApp.
```
load: /examples/point-of-sale/index.rsh
md5:  2d9202924ee606ab47726038fbde05a3
range: 56-59
```
That is all for our `rsh` file.

We can finish with some simple tests for our `{!rsh} API` functions in the frontend `mjs` file.

First, a helper function to generate a random purchase price.
```
load: /examples/point-of-sale/index.mjs
md5:  5229e4cebe07397fe5e73a60dc19f70f
range: 17-20
```
- Line 19 checks if it is the first user, for whom we set a zero test for the `purchasePrice`.

We have used the following testing format for `{!rsh} API` users in other tutorials, so we are going to tackle all of this at once.
```
load: /examples/point-of-sale/index.mjs
md5:  5229e4cebe07397fe5e73a60dc19f70f
range: 22-43
```
- Line 22 is the outer function we'll call from `launched`.
- Line 23 starts the inner function to run individual user actions.
- Line 24-25 are for necessary account and contract tasks.
- Line 26 gets a random purchase price from our `getPurchasePrice()` function.
- Line 27 opts the user in to the Loyalty Token. This is only necessary on AVM networks.
- Line 28 starts a `try...catch` -- we expect our zero cost purchase call to fail.
- Line 29 makes the `{!rsh} API` call to the contract.
- Line 31-33 handles and logs the exception.
- Line 34-37 simulates the first user requesting a refund.
- Lines 40-42 sets up a looping construct to create and run users.

IF we made no mistakes, that is it. Our point-of-sale DApp is ready!

This DApp has no restrictions and will run on different consensus networks.
```cmd
$ ./reach run
```

You should see output that looks like this:
```
Welcome to the point-of-sale machine. This machine processes
payments of varying amounts and rewards each purchase with a loyalty token.
The pos will also process refunds, which return network tokens to the customer,
and loyalty tokens to the contract.

Ready at contract: 0xf7cC1177E46EFC30E6C018c4f5a04bc6A6E860df

Error: Buyer.purchase errored with Error: Assertion failed: Buyer_purchase: sorry, amount too low
  at reach standard library:57:5:application
  at ./index.rsh:37:12:application call to "check" (defined at: reach standard library:49:32:function exp)
  at ./index.rsh:34:39:application call to [unknown function] (defined at: ./index.rsh:34:39:function exp)
  at ./index.rsh:28:45:application call to "runBuyer_purchase0_81" (defined at: ./index.rsh:34:10:function exp)
  at ./index.rsh:28:45:application call to [unknown function] (defined at: ./index.rsh:28:45:function exp)
Purchase number: 1
Customer 1 is getting a refund of 81
Purchase number: 1
Purchase number: 2
Purchase number: 3
Purchase number: 4
Purchase number: 5
Purchase number: 6
Purchase number: 7
Purchase number: 8
Purchase number: 9
Purchase number: 10
Exiting...
```

Done!

If you have completed all of the tutorials up until this point, you are getting pretty good at this. If not, don't worry - practice makes perfect. Plus, we at Reach are never gonna [give you up](https://youtu.be/dQw4w9WgXcQ).

## Bonus Exercise
To understand the Verification Checks and Invariants of this program better, let's break it.

Now that the program is running to completion, go back to your `rsh` file and start commenting out the `{!rsh} check`s and `{!rsh} invariant`s. Nearly every one of them is necessary for this DApp to compile and run. 

Study the compilation errors, understand where they are pointing and why. This is the primary reason we like to include the optional error messages in our `{!rsh} check`s!

Reach out to us in the [Discord](https://discord.gg/kbfNkdaMhD) `#tutorial` channel to tell us you got Rickrolled or brag about your new skills. Just a little though. 

Maybe then tell us about your dreams and aspirations -- or maybe just where you are from. 

Your call.

