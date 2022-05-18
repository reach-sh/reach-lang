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

## {#tut-mkt-interaction} Interaction and Independence

### {#tut-mkt-Interaction-basic} Basic DApp

### {#tut-mkt-Interaction-report} Report Cancellation

### {#tut-mkt-Interaction-other} Other Reports

