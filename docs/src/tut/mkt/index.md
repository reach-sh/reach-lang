# {#tut-mkt} Market Place

## {#tut-mkt-introduction} Introduction
This tutorial is a walk-through on creating a safe application that ensures trust in transacting businesses between individuals.

For the purpose of this tutorial, `seller` and a `buyer` will be the users. And [Reach](https://reach.sh/) is the programming to be employed.

The following image is a pictorial description of how the application will work at the end of this tutorial.

> **Insert Image Describing the flow of transactions**

It begins from building a basic DApp and moves to a more advanced one.
By the end, you will have learnt about the Reach standard library, Participants, Reach Types, variable declaration and definition, steps and so on.

## {#tut-mkt-prerequisite} Prerequisite
To make it easy for you to follow through, it is expected that you have installed Reach. If you need help, use the [Quick Start Guide](https://docs.reach.sh/quickstart/#quickstart).

This tutorial builds on the [Wisdom For Sale]() tutorial. We assume you have finished Wisdom for Sale.

## {#tut-mkt-starter} Starter Code
In this section, the boiler plate for the project will be created.
See full code [here]().

1. Create a folder named `market` in your `Reach` directory:

```reach
mkdir market
```

2.  Create 2 files: `index.rsh` and `index.mjs`

* The `index.mjs` is the frontend of the DApp that will be create since it contains the code the user will interact with.
`Reach` requires this file to compile even if it is empty.

* The `index.rsh` is the backend which contains the DApp's instructions and ensures security of the DApp.
This is actually where our `Reach` code will live.


### {#tut-mkt-starter-backend} Backend

Start working in `index.rsh` by typing out the following code:

```reach
load: /examples/tuts-mkt-1-starter/index.rsh
```

This code isn't doing anything yet but it is a very important foundation to what we want to build. 
Most `Reach` project begins with this structure.

* Line 1 helps `Reach` to decide how to compile or run the DApp.
Without it, you will get an error from the editor.

* Line 3 declares a `commonInteract` object. 
This will hold properties that are common to those participating in this transaction `seller` and `buyer`.

* Line 5 declares a `sellerInteract` object.
 This will hold all properties accessible by `seller`.

* Line 7 declares a `buyerInteract` object.
 This will hold all properties concerning the `buyer`.

* Lines 9 and 19 are the start and end of the main export from the program. 
Only the code within this block is compiled by the compiler. 

* Lines 10 to 13 defines a `{!rsh} Participant` (i.e. `Seller`) and gives the `{!rsh} Participant` access to the `commonInteract` and `sellerInteract` properties.

* Lines 14 to 17 defines another `{!rsh} Participant` (i.e. `Buyer`) and gives the `{!rsh} Participant` access to the `commonInteract` and `buyerInteract` properties.

* Line 18 is used to start executing the program.
Without it, the program will never start.


### {#tut-mkt-starter-frontend} Frontend

Next, work on the `index.mjs` file. Type the  following code:

```reach
load: /examples/tuts-mkt-1-starter/index.mjs
```

* Line 1 imports `{!rsh} loadStdlib` from the Reach standard library.

* Line 2 imports all that is in the `./build/index.main.mjs` file as the project's backend.
This file doesn't exist yet.
It will be automatically generated when you run `./reach compile` or `./reach run` in your terminal.

* Line 3 calls the `loadStdlib` function that was imported on line 1 and the output is stored as `stdlib`.

* Line 5: The `Seller` sets up a contract for the program.

* Line 6: The `Buyer` attaches to the contract.

* Lines 8 to 17 is an asynchronous block that is used to execute the whole of the frontend code.

* Lines 9 to 12 executes the `Seller`'s contract on the participant (i.e. `Seller`). 
The `Seller` is also given access to `commonInteract` and `sellerInteract` properties. 

* Lines 13 to 16 executes the `Buyer`'s contract on the participant (i.e. `Buyer`). 
But this time around, give the `Buyer` access to `commonInteract` and `buyerInteract` properties.

The code that you have just written forms the basis for the DApp that we want to build

Notice that `commonInteract`, `buyerInteract` and `sellerInteract` are similar in both the backend and frontend files. 
This is so because `Reach` is a strongly typed language.
So these variables are declared at the backend before being used at the frontend. 

This might feel like a lot of work especially if you are coming from a language like JavaScript that is weakly typed.
But we would rather use an application with strong security than one with weak security especially when it has to do with money and trust.


### {#tut-mkt-starter-conclusion} Conclusion

This section was aimed at creating the boiler plate for the application that this tutorial plans to build.

Two (2) important files were created. And the necessary code were added.

Next, a simple DApp will be built. This will help to visualize what the finish product would look and feel like.
If the diagram above wasn't clear enough, this next section would help.

Let's G0!


## {#tut-mkt-basic} Basic DApp
In this section a basic DApp will be created to demonstrate what the finish product would look like. 
By the end, you will see how a working `Reach` application looks and feels like.
See full code [here]().


### {#tut-mkt-basic-declare} Declare Variables

Start by fleshing out the constants that we have declared.
Replace this:

```reach
load: /examples/tuts-mkt-1-starter/index.rsh
range: 3 - 7
```

with:

```reach
load: /examples/tuts-mkt-2-basic/index.rsh
range: 3 - 37
```

These are just constants. Nothing surprising:

* Line 3: `choice`. 
This is an `{!rsh} integer` that would represent the product that the buyer decides to order.

:::note
An integer is represented by `{!rsh} UInt`. 
:::

* Line 4: `quantity`. 
This is an `{!rsh} integer` that would represent the quantity of the product that the buyer order.

* Line 5: `announcement`. 
This is a `{!rsh} String` that would represent a short advertisement to the buyer. 

:::note
A string is represented by `{!rsh} Bytes(28)`.
The integer (28) in braces defines how long the string can be.
:::

* Lines 6 to 11: `product`. 
This is an `{!rsh} Object` that contains the properties (i.e. key-value pair) of each product that the `seller` has in stock for sale.

* Line 13: `products`. 
This is an `{!rsh} Array` of `product`. 

* Lines 14 to 25: `commonInteract`.
This would hold properties that would be accessible to both the `seller` and `buyer`.

* Lines 15 to 24: `showResult`.
 This is a  `{!rsh} function` that takes in 2 `Object`s and returns nothing.

:::note
A `{!rsh} function` in Reach is defined as `{!rsh} Fun([input], output)`.
:::

* Lines 27 to 33: `sellerInteract`.
 This would hold all properties accessible by the `seller`.

* Lines 28 to 31: `sellerInfo`.
 This is an `Object` that would contain the `announcement` and `products` that was declared on lines 5 and 13 respectively.

* Line 32: `reportReady`.
 This is a `{!rsh} function` that takes in the `announcement` and `products` and returns nothing.

* Lines 35 to 37: `buyerInteract`.
This would hold all properties accessible by the `buyer`. 

* Line 36: `shop`.
 This is a `{!rsh} function` that represents the buyer's process in looking through the `seller`'s `products` and making a decision as to which one to pick and how many is needed.
 It takes in an `Object` of the `seller`'s `announcement` and `products`. 
 And then returns an `Object` of the `buyer`'s `choice` and `quantity`.

That is all that needs to be done in the backend for now. It now time to move to the frontend.

### {#tut-mkt-basic-accounts} Create Accounts
In the [previous section](##tut-mkt-starter-frontend), contracts were setup for the participants:

```reach
load: /examples/tuts-mkt-1-starter/index.mjs
range: 5 - 6
```

These contracts require that each of the `{!rsh} participant`s have an account setup before entering the contract. Let's do that now. 

Just before the contracts, type the following code:

```reach
load: /examples/tuts-mkt-2-basic/index.mjs
range: 5 - 8
```

* Line 5 sets up some funds that can be used by each `{!rsh} participant` as starting balance.
`{!rsh} parseCurrency` converts the figure passed in to it to an acceptable currency on a DApp.

* Lines 7 and 8 creates an account for each of the `{!rsh} participant`.
This is for test purpose. 

With these accounts, the contracts will compile successfully when called upon.

### {#tut-mkt-basic-define} Define Variables
Having told the backend how those variables declared are to be used, these variables will now be given their proper functionality.

Just below the contracts, enter the following code for the `sellerInteract`:

```reach
load: /examples/tuts-mkt-2-basic/index.mjs
range: 13 - 30
```

Like have already been pointed out, this code will be made accessible to the `Seller` only.

Line 13 defines `sellerInteract` as an object.

Lines 14 to 21 defines the `sellerInfo` as an object.

Line 15 passes a `{!rsh} string` to the `announcement` variable.

Lines 16 - 20: the details of each `product` is passed into the `products` `{!rsh} Array` as `{!rsh} Object`s.
It is a list of items that the `Seller` puts up for sale with their properties.

Lines 22 to 29 creates a  `{!rsh} function` named `reportReady`. 
The `{!rsh} function` takes in `announcement` and `products` as arguments.

Line 23 logs a `{!rsh} string` to the console.

Lines 24 to 28 obtains the contract information which is an `Object` and logs it to the console as a JSON string.
You can see this as the seller's unique identifier.

That is all that the `Seller` will be needing for this section.

The `sellerInteract` output would look like:

```reach
Welcome to the Market
Contract info: {"type":"BigNumber","hex":"0x06"}
```

The next code is for the `buyerInteract`:

```reach
load: /examples/tuts-mkt-2-basic/index.mjs
range: 32 - 47
```

The `Buyer` would have access to this code.

Line 32 defines `buyerInteract` as an `{!rsh} Object`.

Line 33 defines `shop` as a `{!rsh} function`. It takes the `sellerInfo` as an argument.

Line 34 logs the `Seller`'s `announcement` to the console.

Lines 35 to 39 loops through the `Seller`'s `products` and list them out for the `Buyer` to see.

Line 41 gets a random number not more than `4`.
This number stored as `choice` now represent the position of an item in the `products` array.
So if the `choice` is `2`, 
then the `product` chosen will be `{ name: "Corn", unit: "ear", units: "ears", price: "5" }`

Line 41 picks the `quantity` of that `product` the `Buyer` wants.
This is also a random number that is not more than `100`.

Line 43 displays the `name` of the product that the `Buyer` wants to order.

Line 45 returns the `decision` (i.e. the `choice` and `quantity`) of the `Buyer`.

That completes the `buyerInteract`. The output would look like:

```reach
List of products for sale:
1. Potatoes at 10 per bag.
2. Carrots at 10 per bunch.
3. Corn at 5 per ear.
Buyer wants Carrots
```

Finally, the type the code below for `commonInteract`:

```reach
load: /examples/tuts-mkt-2-basic/index.mjs
range: 49 - 61
```

All `{!rsh} participant`s would have access to this code.

Line 49 defines `commonInteract` as a `{!rsh} function` that takes in `person` as an argument.

Line 50 defines `showResult` as a `{!rsh} function` that takes in `decision` (returned on line 45) and `sellerInfo` as an argument. 

Lines 51 to 59 logs to the console the details of the `product` that the `Buyer` has decided to purchase.
The same `product` is also logged to the console as the `product` that the `Seller` has decided to sell.

A more detailed breakdown of the code:

i. `${person === "Seller" ? "sell" : "buy"}` means if the `${person}` is `Seller`, log `sell` else log `buy` to the console.

ii. `${decision.quantity}` is the `quantity` that the `Buyer` wants out of the original quantity of the `product`.

iii. `${decision.quantity > 1 ? sellerInfo.products[decision.choice].units : sellerInfo.products[decision.choice].unit}` is either the `unit` or `units` of the product the `Buyer` chose. This is dependent on the `quantity` that the `Buyer` wants.

iv. `${sellerInfo.products[decision.choice].name}` is the name of the product that the buyer chose.

The `commonInteract` output would look like:

```
Buyer agrees to buy 92 bunches of Carrots
Seller agrees to sell 92 bunches of Carrots
```

That seals the frontend for this project.
Just one more step! 
Go back to the backend file.


### {#tut-mkt-basic-connect} Connect Frontend to Backend

This stage is the last one for the basic DApp. It will make the backend to interact with the frontend and that in turn, will log outputs in the console.

Go back to the backend file. Just below `{!rsh} init();`, type the following code:

```reach
load: /examples/tuts-mkt-2-basic/index.rsh
range: 50 - 55
```

This is called a `{!rsh} local step`. 

A local step is an action that happens on the `{!rsh} participant`'s machine. This step is used to execute codes that are only accessible to a particular `{!rsh} participant`.

* Line 50 calls the `{!rsh} only()` method on the `Seller`.

* Line 51 has the `Seller` `{!rsh} interact` with the `sellerInfo` `{!rsh} Object`. 
Calling `{!rsh} Interact` on something in Reach is the same thing as calling upon a `{!rsh} function` to execute the code within it.

After the `{!rsh} Interact`ion is done, the program is then given the permission to share whatsoever the result of that `{!rsh} Interact`ion is with the public. 
To do this, `declassify()` method is used.

* Line 53 now publicizes the `sellerInfo` that has been declassified.

* Line 54 then `{!rsh} Interact`s with the `Seller`'s `reportReady` `{!rsh} function`. 
The `reportReady` takes in two arguments (i.e. the `Seller`'s `announcement` and `products`)

* Line 55 concludes the `Seller`'s local step by calling `commit()`.

Now, doing a `./reach run` in the terminal would show some output.
That concludes concludes the `Seller`'s interaction.

Next, type in the following code for the `Buyer`'s interaction:

```reach
load: /examples/tuts-mkt-2-basic/index.rsh
range: 57 - 61
```

This is the `Buyer`'s local step

* Line 57 calls the `{!rsh} only()` method on the `Buyer`.

* Line 58 has the `Buyer` `{!rsh} interact` with the `shop` `{!rsh} function`. 
The result of that interaction is also declassified. 

* Line 60 publicizes the `decision` reached on line 58.

* Line 61 concludes the `Buyer`'s steps with a `commit();`

Finally, the result of the program is shown with the following code:

```reach
load: /examples/tuts-mkt-2-basic/index.rsh
range: 63 - 63
```

Line 63 executes the `showResult` `{!rsh} function` on both `{!rsh} participants`.

In the terminal, type `./reach run` and hit `Enter`.
An output similar to the one below show be displayed:

```
Welcome to the Market
Contract info: {"type":"BigNumber","hex":"0x6d"}
List of products for sale:
1. Potatoes at 10 per bag.
2. Carrots at 10 per bunch.
3. Corn at 5 per ear.
Buyer wants Potatoes
Buyer agrees to buy 9 bags of Potatoes
Seller agrees to sell 9 bags of Potatoes



Welcome to the Market
Contract info: {"type":"BigNumber","hex":"0x89"}
List of products for sale:
1. Potatoes at 10 per bag.
2. Carrots at 10 per bunch.
3. Corn at 5 per ear.
Buyer wants Carrots
Buyer agrees to buy 10 bunches of Carrots
Seller agrees to sell 10 bunches of Carrots



Welcome to the Market
Contract info: {"type":"BigNumber","hex":"0x90"}
List of products for sale:
1. Potatoes at 10 per bag.
2. Carrots at 10 per bunch.
3. Corn at 5 per ear.
Buyer wants Carrots
Buyer agrees to buy 81 bunches of Carrots
Seller agrees to sell 81 bunches of Carrots
```

Walah!!! You made it.
You did awesomely.

### {#tut-mkt-basic-conclusion} Conclusion
That was a good way to start building the market place application that we just embarked upon. I hope you found it really helpful and easy to follow through. 

This tutorial has been able to teach the basics of the Reach language. 
Concepts that was covered include, variable declaration and definition, 
the Reach standard library, `{!rsh} Participant`, `{!rsh} Types`, steps and so on.

Congratulations!!! You just became a `Reach developer`.

In the next section, more will be discussed about how `./reach run` works, `process.argv` will be introduced and so on.

## {#tut-mkt-interaction} Interaction and Independence

### {#tut-mkt-Interaction-basic} Basic DApp

### {#tut-mkt-Interaction-report} Report Cancellation

### {#tut-mkt-Interaction-other} Other Reports

