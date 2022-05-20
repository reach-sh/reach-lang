# {#tut-mkt} Market Place

## {#tut-mkt-introduction} Introduction
This tutorial is a walk-through on creating a safe application that ensures trust in transacting businesses between individuals.

For the purpose of this tutorial, `seller` and a `buyer` will be the `{!rsh} Participant`s. And [Reach](https://reach.sh/) is the programming to be employed.

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

* The `index.mjs` is the frontend of the DApp that will be create since it contains the code the `{!rsh} Participant`s will interact with.
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

Time to proceed!


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

These contracts require that each of the `{!rsh} participant`s have an account setup before entering the contract.

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

:::note
This output is just for visualization purpose. 
Some output may not show until a section is completed.
:::

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

It is important to note that Reach offers flexibility in the way code may be written. 
Check [here](/examples/tuts-mkt-3-basic-alt) for an optional way that the code may be written.

This tutorial has been able to teach the basics of the Reach language. 
Concepts that was covered include, variable declaration and definition, 
the Reach standard library, `{!rsh} Participant`, `{!rsh} Types`, steps and so on.

Congratulations!!! You just became a `Reach developer`.

In the next section, more will be discussed about how `./reach run` works, `process.argv` will be introduced and so on.

## {#tut-mkt-interaction} Interaction and Independence
The last section demonstrated what building with Reach feels like. 
But more can be done with Reach than that. 

Reach offers autonomy to `{!rsh} Participant`s in a transaction.
This means that the `Seller` and the `Buyer` could decide whether or not to proceed with the transaction and how they will like the transaction to take place. 

This section would build a similar DApp like the last one but will offer more flexibilities to the `{!rsh} Participant`s.
By the end, how `./reach run` works would have been properly explained. Other things to learn includes what `{!rsh} process.argv` entails, how to handle money and currency and so on.

Hope you are excited about this project?

This section will begin by explaining how `./reach run` works.


### {#tut-mkt-interaction-reach-run} How `./reach run` Works

The `./reach run` command is used to execute whatever is in the `index.rsh` file because in the background, `index.rsh` is added to the command.
So running `./reach run` in the terminal will behave in the same way just as `./reach run index`.

:::note
`./reach run index.rsh` will throw the following error:

```reach
index.rsh.rsh doesn't exist.
```

This is because Reach adds the extension by default already.
:::

`./reach run` produces an array by default. It looks like this:

```reach
[ '/usr/local/bin/node', '/app/index.mjs' ]
```

So any string added to the `./reach run index` command, would be added to that array.
For example, `./reach run index seller` would produce:

```reach
[ '/usr/local/bin/node', '/app/index.mjs', 'seller' ]
```

The item(s) that come after the second item in the array, tells us the variable that have been passed in.

To see this working, type the following on line 4 of the `index.mjs` file:

```reach
console.log(process.argv);
```

Run `./reach run index seller` in the terminal.

The output it produces would look like:

```reach
[ '/usr/local/bin/node', '/app/index.mjs', 'seller' ]
Welcome to the Market
Contract info: {"type":"BigNumber","hex":"0xba"}
List of products for sale:
1. Potatoes at 10 per bag.
2. Carrots at 10 per bunch.
3. Corn at 5 per ear.
Buyer wants Potatoes
Buyer agrees to buy 95 bags of Potatoes
Seller agrees to sell 95 bags of Potatoes
```

The first line of the output is the line of interest. 
That will be leveraged in giving the `{!rsh} Participant`s autonomy as the project for this section is developed.

That is the basics of how the `./reach run` command works. 

### {#tut-mkt-Interaction-basic} Basic Interactive DApp
In other to make it easy to follow, delete all the codes in the two files.

Type the following code in the backend file:

```reach
load: /examples/tuts-mkt-4-interaction-basic/index.rsh
range: 1 - 34
```

This code looks familiar. A few changes were made:

* `choice` and `quantity` has been removed.

* `commonInteract` is currently empty. 
It has also been destructured into the `sellerInteract` and `buyerInteract` `{!rsh} Object`.
This will produce the same result as before when it was destructured inside of the `{!rsh} Participant` definition.

* `shop` `{!rsh} function` now returns an `{!rsh} Object` containing `prodNum` and `prodAmt` instead of `choice` and `quantity`.

* Since `commonInteract` has already been destructured into the `sellerInteract` and `buyerInteract` `{!rsh} Object`, 
`sellerInteract` and `buyerInteract` was passed to the `{!rsh} Participant` definition without destructuring.

* Finally, `Seller` and `buyer` has been changed to `S` and `B`.

That's all for the backend for now. Go to the frontend.

In the `index.mjs` file, type the code below:

```reach
load: /examples/tuts-mkt-4-interaction-basic/index.mjs
range: 1 - 12
```

* Line 1 imports the `{!rsh} ask` `{!rsh} Object` alongside `{!rsh} loadStdlib` from the Reach standard library.

* Line 3 calls on the `{!rsh} loadStdlib` `{!rsh} function` and passes the `{!rsh} process.env` as an argument.

* Lines 5 to 8 is a conditional statement that checks if there is no third argument passed to the `./reach run` command or if the third argument passed is `seller` or `buyer`.

* Line 6 logs a message telling the `{!rsh} Participant` what command to run for the program to work if line 5 evaluates to `{!rsh} true`.

* Line 7 terminates the program.

* Line 10 stores the third argument passed to the `./reach run` command as `role`

* Line 11 reveals the role that the `{!rsh} Participant` chose.

* Line 12 reveals the consensus network that the `{!rsh} Participant` chose. 


After that, type in the following:

```reach
load: /examples/tuts-mkt-4-interaction-basic/index.mjs
range: 14 - 29
```

* Line 14 accesses the standard unit through the standard library based on the consensus network and stores it as stored as `suStr`.

* Line 15 tells the `{!rsh} Participant` what standard unit is being used.

:::note
A standard unit is the network token unit most commonly associated with a network. 
For example, the standard unit of `Ethereum` is `ETH`. 
:::

* Line 16 accesses the atomic unit through the standard library based on the consensus network and stores it as stored as `auStr`.

* Line 17 tells the `{!rsh} Participant` what atomic unit is being used.

:::note
An atomic unit on the other hand, is the smallest unit of measure for the standard unit. 
It cannot be divided into smaller units.
For example, the atomic unit of `Ethereum` is `WEI`. 
:::

* Line 18 is a `{!rsh} function` that converts money from its standard unit to its atomic unit.

* Line 19 is a `{!rsh} function` that converts money from its atomic unit to its standard unit.

* Line 20 sets up a standard unit balance of 1000.

* Line 21 displays the standard unit balance.

* Line 22 converts the standard unit balance to it's atomic unit.

* Line 23 displays the atomic unit balance.

* Line 24 converts the atomic unit balance back to the standard unit balance and displays it to the `{!rsh} Participant`.

* Line 19 is a `{!rsh} function` that returns the account balance of a particular `{!rsh} Participant`.

* Line 27 creates a test account.

* Line 29 defines the `commonInteract` `{!rsh} Object`.

The code so far has defined utilities for the DApp. 
The next thing to do is to utilize these utilities in defining what happens if the `{!rsh} Participant` is a `seller` or a `buyer`.


#### {#tut-mkt-Interaction-basic-seller} Seller
The code below would be executed if the `{!rsh} Participant` is the `seller`:

```reach
load: /examples/tuts-mkt-4-interaction-basic/index.mjs
range: 32 - 52
```

* Line 32 checks if the `{!rsh} Participant` is a `seller`

* Lines 33 to 47 looks familiar. This block defines the `sellerInteract` `{!rsh} Object`.

* Line 34 destructures the `commonInteract`

* Lines 35 - 42 defines the `sellerInfo` `{!rsh} Object`.

* Lines 43 to 46 defines the `reportReady` `{!rsh} function`.

* Line 49 displays the account balance of the `seller` before the transaction begins.

* Line 50 sets up the contract for the transaction.

* Line 51 executes the contract on the `seller`. 
It also gives the `seller` access to the `sellerInteract` properties.

* Line 52 displays the account balance of the `seller` after the transaction ends.

With that line, the `seller`'s part is done.

The `seller`'s output would look like:

```reach
Your role is seller
The consensus network is ALGO.
The standard unit is ALGO
The atomic unit is μALGO
Balance is 1000 ALGO
Balance is 1000000000 μALGO
Balance is 1000 ALGO
Your Balance is 1000 ALGO.
Welcome to the Market
Contract info: {"type":"BigNumber","hex":"0xc2"}
```

#### {#tut-mkt-Interaction-basic-buyer} Buyer
If the `{!rsh} Participant` is not a `seller`, then the following code would be executed.
Type the following:

```reach
load: /examples/tuts-mkt-4-interaction-basic/index.mjs
range: 55 - 78
```

* Line 55 checks if the `{!rsh} Participant` is not a `seller`.

* Lines 56 to 66 defines the `buyerInteract` `{!rsh} Object`. And that also looks familiar.

* Line 57 destructures the `commonInteract` `{!rsh} Object`.

* Lines 58 to 65 defines the `shop` `{!rsh} function`.

* Line 68 uses the `{!rsh} ask` `{!rsh} Object` imported on line 1 to request for the contract details to be pasted in the terminal.


The `buyer`'s output at this point looks like:

```reach
Your role is buyer
The consensus network is ALGO.
The standard unit is ALGO
The atomic unit is μALGO
Balance is 1000 ALGO
Balance is 1000000000 μALGO
Balance is 1000 ALGO
Paste contract info: 
```


* Lines 69 and 70 is executed after the `buyer` has pasted the details of the contract in the terminal

* Line 71 attaches the `buyer` to the contract.

* Line 72 displays a success message

* Line 73 displays the account balance of the `buyer` before the transaction begins.

* Line 74 executes the contract on the `buyer`. 
It also gives the `seller` access to the `buyerInteract` properties.

* Line 75 displays the account balance of the `buyer` after the transaction ends.

* Line 77 ends the usage of the `{!rsh} ask` `{!rsh} Object`.

That ends the buyer's transaction

The `buyer`'s output now looks like:

```reach
Your role is buyer
The consensus network is ALGO.
The standard unit is ALGO
The atomic unit is μALGO
Balance is 1000 ALGO
Balance is 1000000000 μALGO
Balance is 1000 ALGO
Paste contract info: 
{"type":"BigNumber","hex":"0xc2"}
Attaching to contract
...
Successfully attached
Your Balance is 1000 ALGO.
List of products for sale:
1. Potatoes at 200 ALGO per unit (bag).
2. Carrots at 100 ALGO per unit (bunch).
3. Corn at 50 ALGO per unit (ear).
Your Balance is 999.998 ALGO.
```

While the `seller`'s output would look like:

```reach
Your role is seller
The consensus network is ALGO.
The standard unit is ALGO
The atomic unit is μALGO
Balance is 1000 ALGO
Balance is 1000000000 μALGO
Balance is 1000 ALGO
Your Balance is 1000 ALGO.
Welcome to the Market
Contract info: {"type":"BigNumber","hex":"0xc2"}
Your Balance is 999.997 ALGO.
```


#### {#tut-mkt-Interaction-connect} Connect the frontend to the backend
To bring everything together, head back to the `index.rsh` file.
Type the following code after the init line for the `seller`:

```reach
load: /examples/tuts-mkt-4-interaction-basic/index.rsh
range: 36 - 41
```
The only difference here is that `Seller` has been changed to `S`. 
Every other thing remains the same.

Type the code below for the `buyer`:

```reach
load: /examples/tuts-mkt-4-interaction-basic/index.rsh
range: 43 - 55
```

Lines 44 and 46: `decision` was changed to `order`.

Line 47 checks if the `prodNum` is zero (0) or `prodNum` is more than the number of items for sale or `prodAmt` is zero (0).

Lines 48 and 49 `{!rsh} commit`s and `{!rsh} exit`s the program respectively if line 47 evaluates to `{!rsh} true`.

Line 51 `{!rsh} commit`s to the program if line 47 evaluates to `{!rsh} false`.

Line 54 terminates the program.

#### {#tut-mkt-Interaction-test} Testing

#### {#tut-mkt-Interaction-conclusion} Conclusion

### {#tut-mkt-Interaction-report} Report Cancellation

### {#tut-mkt-Interaction-other} Other Reports

