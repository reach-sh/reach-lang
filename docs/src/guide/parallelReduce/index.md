# {#guide-parallelReduce} parallelReduce Guide
The purpose of this guide is to bring together all of the information we have about the parallelReduce and to provide additional clarity on how it works and why we use it.

## Description
There are layers of abstraction to the `{!rsh} parallelReduce` structure that we should understand first.  

Generally speaking it is an abbreviation of a `{!rsh} while` loop pattern. 

The variables you set are repeatedly updated uniquely by each one of your participants until the loop condition no longer holds.
Values are updated via `{!rsh} return` statements, while `{!rsh} continue` is implicit.

If some thing or many things can be done by many different people, you use a `{!rsh} parallelReduce` to handle the `{!rsh} fork` and cases.

A `{!rsh} fork` is a race between API members where we are unsure who exactly is performing the next consensus operation, this is why they are common with API members – which offer functionality to many different participants. 

Fork ends in the execution of a switch structure, executing the appropriate case (if Bob1 wins do this, if Bob2 wins do that). Read more about `{!rsh} fork`, including the underlying code that makes up the fork keyword.

:::note
Learn more about `{!rsh} race` in the [race guide](##guide-race).
:::

Read through the [guide about when to use what kind of consensus transfer](##guide-ctransfers).

After you understand that you can examine the structure of `{!rsh} parallelReduce` components and their definitions.

Let's start by looking at the most simple `{!rsh} parallelReduce` that we could implement.
```
load: /examples/parallelReduce-api_-counter/index.rsh
md5: 5a60138c737baf125f0cdccec40fec3e
range: 8-10
```
Here we declare one API member function `countUp` for the API on line 9, then define its functionality below.
```
load: /examples/parallelReduce-api_-counter/index.rsh
md5: 5a60138c737baf125f0cdccec40fec3e
range: 18-26
```
- Line 18 declares one loop variable `count` and initializes it to zero, starting the `{!rsh} parallelReduce`.
- Line 19 sets the invariant, this contract will accept no tokens.
- Line 20 is a standard `{!rsh} while` loop condition, it runs until the condition breaks.
- Line 21 is the definition of our API member function `countUp`
`API.functionName` and takes zero arguments.
- Line 22 starts the outer return and zero is in the pay expression. This function takes no payment from the user. You can specify any number to be paid by the user here. You can also omit the zero and Reach will synthesize this to zero. `ret` is the return function to return the function signature value to the caller. Here we have said that the return value is a UInt on line 8. More information on this below.
- Line 23 invokes that return function and returns the `count`.
- Line 24 updates the `count` loop variable to `newCount`.

### API Inputs and Returns
All API member functions must rely only on consensus state and the function inputs.

The API member function has a consensus reduction specification function that takes an argument. Here that function is called `ret`. 

You can name this function anything you like, traditionally `ret` or `k` is used to denote return or continuation, respectively.
```
load: /examples/rsvp-6-vevt/index.rsh
md5: ed7d96413f2f23224d5ea6081ae4cc78
range: 52-62
```
`ret` must be called inside the API member function to provide the return value to the API caller. Below on line 17 is the function signature for the API member function `register`. It takes no arguments and returns `{!rsh} null`.
```
load: /examples/rsvp-6-vevt/index.rsh
md5: ed7d96413f2f23224d5ea6081ae4cc78
range: 16-18
```
And here again is the implementation of the `register` function, this time pointing to the inputs and outputs.  

Remember, it takes no arguments and returns `{!rsh} null`.
```
load: /examples/rsvp-6-vevt/index.rsh
md5: ed7d96413f2f23224d5ea6081ae4cc78
range: 52-62
```
- Line 52 specifies the API and which function we are referencing `Guest.register`. The empty parenthesis denotes that it takes no parameters.
- Line 59 invokes our `ret` function and returns a `{!rsh} null` value to the caller.

::::testQ
As an exercise -- bring the `index.rsh` file to your machine and change the `ret(null)` on line 59 to `ret(true)` and see what error comes up.

:::testA
You should get a type mismatch error, because the return does not match the function signature. @{seclink("RE0088")}
:::
::::

Now that we understand our loop, how to update values, and how to structure function inputs and outputs -- let's look at the difference in syntax between `.api` and `.api_`.

### .api_ vs .api Syntax

#### .api_

We will demonstrate our `countUp` function with both `.api` and `.api_`. 

In the following examples these two functions have the same funtionality, to increase the count by one each time a user calls our function.

First, the `.api_` that you have seen already.
```
load: /examples/parallelReduce-api_-counter/index.rsh
md5: 5a60138c737baf125f0cdccec40fec3e
range: 21-26
```
- Line 21 denotes the start of our API member function `B.countUp`. The empty parenthesis means it takes no arguments.
- Line 22 starts the outer return. Zero denotes the amount to be paid and we declare our `ret` function.
- Line 23 invokes our ret function and returns the value of `count` to the caller.
- Line 24 updates the loop variable `count` increasing it by one.

Any checks performed before the `{!rsh} return` statement will be applied in the local step, during payment and the consensus step of the API call. You should use `{!rsh} .api_` when this check is the same for these steps.

#### .api

Now notice the difference in syntax for `.api`. Remember, this function does the same thing as the one above.
```
load: /examples/parallelReduce-api-counter/index.rsh
md5: 0fee750111780031c4fb176b431e862e
range: 21-27
```
- Line 21 denoteds the start of our API member function `B.countUp`.
- Line 22 is for the `ASSUME_EXPR`. The empty parenthesis means it takes no arguments. These will be evaluated in a local step, so you may use it to add things you `{!rsh} assume` about the values given by the api caller.
- Line 23 is the pay expression, it is set to zero, matching our previous function.
- Line 24 declares our return function `ret`.
- Line 25 invokes that return, sending `count` back to the caller.
- Line 26 updates our loop variable `count`.

A key difference now is the ability to specify different verification checks for the different Reach modes your function will go through.

You could have an `{!rsh} assume` expression that evaluates in the users local step and a different expression to `{!rsh} require` in a consensus step.

Read through both of these functions again, this time comparing syntax for the same operation in each function.

You see that `{!rsh} .api_` is an abstraction of `{!rsh} .api` with more compact syntax and `{!rsh} .api` allows you to specify different verification checks at different steps of the function.

Now that we are comfortable with the syntax -- let's look at accessing some values.

### .define your Views

The `{!rsh} parallelReduce` operates in a consensus step and the values that we are tracking are already publicly accessible. 

To easily view any of our values, we should declare them as `{!rsh} Views` and include them in the `{!rsh} .define` block of our `{!rsh} parallelReduce`.

In the following examples from [rsvp-6-vevt](https://github.com/reach-sh/reach-lang/blob/master/examples/rsvp-6-vevt/index.rsh) we are tracking `howMany` users have called our `register` function.
```
load: /examples/rsvp-6-vevt/index.rsh
md5: ed7d96413f2f23224d5ea6081ae4cc78
range: 44-51
```
- Line 44 declares our loop variables.
- Line 45 initializes values for those variables inside our `{!rsh} parallelReduce`.
- Line 46 starts the `{!rsh} .define` block.
- Line 47 is where we set the `{!rsh} View` related to our value. In this case, `howMany` users have registered.
- Lines 49-50 are for our [loop invariants](##guide-loop-invs).
- Line 51 is our loop condition.

The `{!rsh} parallelReduce` is going to leave our DApp in a state where API members can call our function and update the values, but also allow `howMany` to be accesible in the frontend with a call to the `{!rsh} View`.

More information on accessing `{!rsh} Views` from the frontend can be found at `{!js} ctc.views`.

Now, what if we want the user to pay some amount to access this function?

## Pay the Pied ParallelReduce

For network tokens, we can just change the zero from our counter program to some amount.

In the following example from [ticket-sales](https://github.com/reach-sh/reach-lang/blob/master/examples/ticket-sales/index.rsh), the seller first sets some `cost` for the ticket and we include this for the `PAY_EXPR`.
```
load: /examples/ticket-sales/index.rsh
md5: c425745032273893d106fe3de005f15e
range: 29-36
```
- Line 31 is the outer return and will prompt the user to pay `cost` at their wallet interface to execute the consensus code.

In the [rsvp-6-vevt](https://github.com/reach-sh/reach-lang/blob/master/examples/rsvp-6-vevt/index.rsh) example, we ask the user to pay `reservation`.
```
load: /examples/rsvp-6-vevt/index.rsh
md5: ed7d96413f2f23224d5ea6081ae4cc78
range: 52-62
```
- Line 55 starts the outer return and asks the user to pay `reservation` before executing the consesus step.

All of our examples have so far demonstrated using network tokens. But what if you need the user to pay the contract in a non-network token?

#### Non-network Tokens

Any token that is not the native token of a protocol is considered a non-network token.

The gas or txn fees, are paid in network tokens. 

Examples of non-network tokens include all Contract Tokens  on ETH (ERC20, ERC721, etc..) and all ASA Tokens on Algorand based networks.
@{seclink("guide-nntoks")}

First, we need to teach the Smart Contract about our non-network Token. This means providing it from the frontend (usually the Admin does this) -- and `{!rsh} publish`ing this token ID.

:::note
On ETH the token ID is an Address.
On Algorand it is a unique ID number.
:::

We will examine the [point-of-sale](https://github.com/reach-sh/reach-lang/blob/master/examples/point-of-sale/index.rsh) example for this.
```
load: /examples/point-of-sale/index.rsh
md5: 2d9202924ee606ab47726038fbde05a3
range: 19-24
```
- Line 19 starts a local step for `A`.
- Line 20 unpacks values from the frontend `params` object.
- Line 22 `{!rsh} publish`es these values to the blockchain.

:::note
We cannot attach the `A.pay([[supply, tok]])` to the first `{!rsh} publish` of the DApp. This is because the Smart Contract doesn't yet know of our token. We first need to complete a `{!rsh} publish` of the token ID.

Reach supports network tokens by default, which is why this constraint does not exist when paying network tokens.
:::

You may have noticed the syntactic Tuple in the `{!rsh} .pay`. 

Token IDs and the amounts to be paid need to be specified in `{!rsh} Tuples`. 

The first argument is the default network token and is synthesized for you, just specify the quantity -- if you leave the quantity out it will be synthesized to zero for you.

This means `{!rsh} A.pay([[supply, tok]])` is equal to `{!rsh} A.pay([0, [supply, tok]])`. Where zero specifies the number of network tokens. 

This syntax will come up again.

This particular program sells these as loyalty tokens, but also allows the user to return the token in the event of a refund. 

This means that our `{!rsh} parallelReduce` will need to *accept* these non-network tokens as payment in our API member function.
```
load: /examples/point-of-sale/index.rsh
md5: 2d9202924ee606ab47726038fbde05a3
range: 28-33
```
- Line 28 starts our `{!rsh} parallelReduce`.
- Line 29 is new. `{!rsh} .paySpec([tok])` tells our `{!rsh} parallelReduce` that it can also accept payment in this non-network token.

:::note
This is structured as an array, so you can just list out the other tokens seperated by a comma.

`.paySpec([tok1, tok2])`

Just be sure they have been `publish`ed and you have Reach `assume(tok1 != tok2)` for each token.
:::

```
load: /examples/point-of-sale/index.rsh
md5: 2d9202924ee606ab47726038fbde05a3
range: 34-45
```
- Line 34 specifies the `API_EXPR` and takes an input `purchasePrice` from the caller.
- Lines 35-37 perform various verification checks.
- Line 38 starts our outer return. This is where we need to specify the `PAY_EXPR`, but it also needs to account for our non-network token syntax. `[purchasePrice, [0, tok]]` has the user pay the `purchasePrice` in network tokens and zero of our non-network token.

Now let's accept our non-network token from the user in the event of a refund.
```
load: /examples/point-of-sale/index.rsh
md5: 2d9202924ee606ab47726038fbde05a3
range: 46-55
```
- Line 48 is our outer return. Notice the same syntax as the `purchase` function, but with different amounts. Now one `tok` is coming into the contract upon approval of the transaction.

Hopefully this guide has helped you better understand the `{!rsh} parallelReduce`.

If you still have questions, Reach out to us in [Discord](https://discord.gg/kbfNkdaMhD)!

