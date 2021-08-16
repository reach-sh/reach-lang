



# {#overview} Overview

This is an informal overview of Reach and the structure of a Reach program.
The goal of this document is to give enough technical specifics to help you understand what Reach does, but it isn't intended as either a [tutorial](##tut) or a [reference](##ref).
When you're ready to really begin a project, you can start with one of those, or [the workshop](##workshop).

If you have experience with blockchain development using existing tools, we recommend reading this article and [a comparison with other development platforms](##guide-solidity).

::: note
A recording of a live workshop that goes over this material is [available on YouTube](https://www.youtube.com/watch?v=1jmi8XdOvD4).
:::

## {#over-model} Decentralized applications

DApps are made of multiple agents interacting with each other through some backend consensus network, like Ethereum or Algorand.
These agents act on behalf of principals that provide direction and authority through information.
These principals might be humans or other autonomous agents or even committees and organizations with their own structure.
The consensus network allows these agents to transfer and receive value in the form of network-specific tokens, like ETH or ALGO.
The network also allows the creation of "contracts" that ensure that all agents follow the same rules as they take turns computing and publishing values and information.
The details of these "contracts" are specific to each consensus network, but they are implicitly trusted by all agents and principals because their operation can be independently verified to match the previously agreed-upon rules.

A single Reach program incorporates all aspects of a DApp:
+ Participant backends are the agents acting on behalf of the principals.
+ Frontends are the technical representation of the interface between the participants and the principals.
+ A contract enforces the rules of the program, including the order of operation.


In Reach, a programmer only needs to specify the actions of participants---what they do individually and what they do in unison.
The Reach compiler automatically derives a contract for the consensus network via a connector that enforces these rules.

## {#over-minimal} A minimal Reach program

Let's look at a simple Reach program where two principals, Alice and Bob, interact. In this DApp, Alice has some information that Bob might want and she has an amount of network tokens in mind that she'd like to trade for it.

::: note
You can look at the entire example program by visiting [overview/index.rsh](@github/examples/overview/index.rsh).
:::

::: note
Get language support for Reach in your editor by visiting [the guide on editor support](##guide-editor-support).
:::

The main part of the program looks like this:

@[code{1-15}](@reach-lang/examples/overview/index.rsh)

+ Line 1 specifies that this is a Reach program.
+ Line 2 specifies that this program will be compiled with strict mode, which enables unused variable checks.
+ Line 4 defines the main export from program. `main` is the default used by Reach.
+ Line 4 also specifies that it is an application.
+ Line 5 specifies that the program identifier `A` will represent the Alice participant.
+ Lines 6 and 7 specify the interface between Alice's participant and frontend. In this case, Alice's frontend must provide a number called `request` and a string called `info`.
+ Line 9 specifies that the program identifier `B` will represent the Bob participant.
+ Lines 10 and 11 specify the interface for Bob, which includes a function named `want`, that takes a number and returns `null`, as well as a function named `got`, that receives the information.
+ Finally, line 13, deploys the DApp.


The elided lines, 14 through 34, contain the body of the application, which we can divide into four parts.

@[code{15-18}](@reach-lang/examples/overview/index.rsh)

+ Lines 15 and 16 specify that Alice takes a local step where she declassifies the amount of tokens requested.
In Reach, all values from the frontend are secret until explicitly made public with declassify.
+ Line 17 has Alice join the application by publishing that value, and the logic of the program transitions to specifying what the contract does.
+ Line 18 has the contract commit to these values and continue the rest of the program.


At this point, Bob's backend has learned the value of `request` and can deliver it to Bob's frontend for his approval. This happens next.

@[code{20-23}](@reach-lang/examples/overview/index.rsh)

+ Lines 20 and 21 have Bob perform that delivery.
`interact.want` doesn't explicitly return a boolean because the frontend cannot return if Bob doesn't want to continue.
A better version of this program might return `false` and have that communicated to Alice.
+ Lines 22 and 23 have Bob join the application and submit a payment matching the appropriate amount, and then the contract commits.


It's now Alice's turn again,

@[code{25-29}](@reach-lang/examples/overview/index.rsh)

+ Lines 25 and 26 specify that Alice declassifies the information.
+ Line 27 has her publish it.
+ Line 28 has the contract transfer the requested amount to her.
+ Line 29 commits the transactions on the consensus network.


The only thing left is for Bob's backend to deliver the information to his frontend.

@[code{31-33}](@reach-lang/examples/overview/index.rsh)

+ Line 31 and 32 do this.
+ Line 33 exits the program.


---

Reach programmers don't need to think about details like _contract storage_, _protocol diagrams_, _state validation_, or _network details_; instead, they can focus exclusively on the business logic of their application.

## {#over-compile} Compile

After a Reach programmer writes this application in a file like [`overview/index.rsh`](@github/examples/overview/index.rsh), they could run

```
$ reach compile [overview/index.rsh](@github/examples/overview/index.rsh)
```


and the `build` directory will contain a new file named [`index.main.mjs`](@github/examples/overview/build/index.main.mjs), which contains a JavaScript implementation of a backend for each participant, as well as the Ethereum bytecode for the contract.

::: note
If you are curious, you can take a look at this file by going to [overview/build/index.main.mjs](@github/examples/overview/build/index.main.mjs).
The Ethereum bytecode is not readable, but if you understand Solidity, you may want to look at [overview/build/index.main.sol](@github/examples/overview/build/index.main.sol) to see the original Solidity source that it is compiled from.
Reach can leave files like these in place when run with `--intermediate-files`.
:::

For this thirty line application, the Reach compiler generated hundreds of lines of JavaScript code in two functions, one for Alice and one for Bob.
Separately, it generated over hundreds of lines of Solidity code to implement the contract.
If a programmer wasn't using Reach, they would have to write all this code in these three modules separately and keep them synchronized at every step of the development process.

Moreover, Reach doesn't only work for Ethereum: it is blockchain agnostic and can be easily configured to use a different connector to target a different consensus network, like Algorand.
Nor is Reach tied to JavaScript: it can be configured to target other backend languages, like Go.

## {#over-verify} Verify

Reach doesn't just compile your program: it also verifies it and ensures that entire categories of errors don't occur.
For example, it always guarantees that the balance in the contract at the end of the program is zero.
This is important because if it were not true, then tokens would be locked away by the contract and inaccessible.

For this example program, it is obvious that when a single transfer of `request` goes in at line 22 and a single transfer of `request` goes out at line 28, then the balance is zero at the end of the program.
We could make a small tweak, however, to demonstrate things going wrong.

Let's change the third step to leave a single unit in the balance:

@[code{25-29}](@reach-lang/examples/overview/index-error.rsh)

And then run the compiler:

```
$ reach compile [overview/index-error.rsh](@github/examples/overview/index-error.rsh)
```


It will print out a detailed error message showing the violation.

@[code{2-28}](@reach-lang/examples/overview/index-error.txt)

Verification failures include a lot of information, such as a concrete counter-example showing values that could have been provided by frontends that would lead to the property failing to hold.
In this case, it reports that if Alice were to pass an `interact.request` over `1` at the start of the program on line 5, then the balance of the contract would not be provably `0` at the end of the program.

---

Reach programmers don't need to worry about entire categories of errors because the compiler automatically checks their code and ensures that those errors aren't present.
Of course, there's a lot more to say about the details of [automatic verification](##guide-assert); indeed, it is one of the most powerful features of Reach, but we'll leave it at that for now.

## {#over-interface} Interface

The backend produced by the Reach compiler isn't an application on its own.
In particular, each participant needs a frontend to interact with.
In a real deployment, this interfacing code would be tied to a GUI, like a Web or smartphone app.
Let's look at a simple command-line version that demonstrates how it would work for testing on a private devnet.

::: note
You can look at the entire example interface program by visiting [overview/index.mjs](@github/examples/overview/index.mjs).
:::

The program is just a few dozen lines long and the shell of it is quite simple:

@[code](@reach-lang/examples/overview/index.mjs)

+ Lines 1 and 2 import the Reach standard library loader and the compiled app backend.
+ Line 5 dynamically loads the appropriate network-specific Reach standard library,
based on the `REACH_CONNECTOR_MODE` environment variable.
If unspecified, Reach's Ethereum standard library will be used by default.
All of Reach's network-specific standard libraries adhere to a common interface allowing you to write programs that are network-agnostic.
+ Lines 7 and 8 initialize new test accounts for Alice and Bob.
+ Line 10 has Alice deploy the contract on the consensus network.
+ Line 11 has Bob attach to the contract.
The value `ctcAlice` contains no secret information and could easily be printed out and shared with Bob outside of the consensus network.
+ Lines 13 through 22 launch the backends and wait for their completion. We'll look at the details in a moment.


This code, similar for all test programs, demonstrates how straightforward it is to scaffold a Reach application for testing.

Let's look at initializing and interfacing each participant, starting with Alice.

@[code{14-17}](@reach-lang/examples/overview/index.mjs)

+ Line 14 invokes Alice, passing a contract object which includes the standard library used by the backend to interface with the consensus network. 
+ Line 15 provides the `request` value.
+ Line 16 provides the `info` value.


Let's look at Bob next.

@[code{18-21}](@reach-lang/examples/overview/index.mjs)

+ Line 18 initializes Bob just like Alice.
+ Line 19 provides his `want` function, which produces a log message and always accepts.
+ Line 20 provides his `got` function, which displays the secret on the console as well.


---

Reach completely abstracts all the details of the chosen consensus network from the programmer, except for those directly impinging on business decisions, like the amounts of currency transacted.
Reach allows programmers to focus on the business logic of their application at every stage, from the core application to the interfacing elements.

## {#over-execute} Execute

It's now time to execute this test program and ensure that everything is working correctly.
In this case, we've set up our application simply: there's one Reach file for the application and one JavaScript file for the interface.
This is a common practice, so Reach comes with a simple wrapper script to build and execute such applications.
We just run:

```
$ reach run
```


And then Reach

+ compiles [overview/index.rsh](@github/examples/overview/index.rsh);
+ creates a temporary Node.js package;
+ builds a Docker image based on Reach's standard image for the package; and,
+ runs the application connected to Reach's standard private Ethereum devnet image.


On typical developer laptops, this entire process takes seconds and can be completely integrated into existing development IDEs, like VSCode, so Reach developers can compile, verify, build, launch, and test their Reach app with a single command.

---

Reach completely abstracts all the details of building and maintaining consensus network test environments and build scripts from the programmer, so they can focus exclusively on the business logic of their application.
In fact, Reach works for multiple networks, so if we instead run

```
$ REACH_CONNECTOR_MODE=ALGO reach run
```


Then Reach will instead start up a private Algorand devnet image and use the Algorand connector.
The developer does not need to change anything about their program because Reach is entirely agnostic to the consensus network choice during deployment.

The same goes for Conflux:

```
$ REACH_CONNECTOR_MODE=CFX reach run
```


## {#over-react} Web app

::: note
You can [watch a 7-minute video](https://www.youtube.com/watch?v=jHEKIMNvs-o) on YouTube
which demonstrates this section's code in action
and provides a brief explanation of how it works.
:::

The previous execution uses Node.js to perform a test run at the command line.
However, most Reach developers deploy their DApps via a Web application, as we describe below.

A Web deployment uses the exact same [index.rsh](@github/examples/overview-react/index.rsh) file connected, this time, to a React-based [index.js](@github/examples/overview-react/index.js) file.
(It also uses some simple React [views](@github/examples/overview-react/views)
 and [`css`](@github/examples/overview-react/index.css) to go with it.)
Let's take a look at some snippets from the React [index.js](@github/examples/overview-react/index.js) and compare with the Node.js [index.mjs](@github/examples/overview/index.mjs) from before:

@[code{7-9}](@reach-lang/examples/overview-react/index.js)

At the top of the file, we import the Reach-generated backend as `backend` and we load the standard library as `reach`.

@[code{27-28}](@reach-lang/examples/overview-react/index.js)

We hook into the App [component](https://reactjs.org/docs/react-component.html)'s [lifecycle event](https://projects.wojtekmaj.pl/react-lifecycle-methods-diagram/) `componentDidMount`
in order to fetch the user's account.
`getDefaultAccount` automatically interacts with browser extensions, like MetaMask, to get the user's
currently-selected account.
Reach is able to deploy contracts and send transactions to the consensus network by prompting the user directly through the extension's API, without additional assistance from the React frontend.
This is just like how in the Node.js deployment, the Reach programmer does not need to decode the details of the underlying consensus network's interaction API.

@[code{71-76}](@reach-lang/examples/overview-react/index.js)

Our React component has a method called `deploy` that actually deploys the contract on the network, using the same calls as in the test deployment:
on line 72 we call the `acc.deploy` function,
and on line 74, we call the `ctc.getInfo` function;
exactly as we did for the Node.js program.

@[code{79-85}](@reach-lang/examples/overview-react/index.js)

Similarly, we implement a `runBackend` method that executes the Reach program as Alice using information gathered from the React UI.

@[code{112-121}](@reach-lang/examples/overview-react/index.js)

We implement a similar method in the `Bob` component that runs the backend as Bob.

We specify Alice's and Bob's respective participant interact interfaces
just as we would in Node.js.
In the React program,
we have the ability to leverage Bob's `interact` functions as callbacks
that can update the React state
in order to display to, or harvest information from, the React user interface.

You can install the `@reachsh/stdlib` JavaScript library
into your React project,
or for convenience, instead of setting up a React project,
you can simply use the command

```
$ reach react
```


This command runs your DApp with the React development server in a Docker container which has Reach and React JavaScript dependencies pre-installed, so it starts up much faster than building them yourself.

As before, you can use `REACH_CONNECTOR_MODE` to  choose your desired connector.

```
$ REACH_CONNECTOR_MODE=ETH reach react
```

```
$ REACH_CONNECTOR_MODE=ALGO reach react
```

```
$ REACH_CONNECTOR_MODE=CFX reach react
```


## {#over-next} Next steps

In this overview, we've briefly described the structure and fundamental concepts of a Reach application.
We've shown how to construct a simple program, compile it, connect an interface, test at the command-line, and deploy it using a React Web application.
Since this is only a brief overview of what Reach can do, we left a lot out.
But even so, it should be clear why Reach is the easiest and safest programming language for decentralized application development.

Furthermore, this example program has many flaws and should not be used in practice.
For example, it provides no protection to Bob in the event that Alice fails to deliver the information, and makes no attempt to ensure that the information is what he wants.
Reach allows you to abstract away the low-level details of your decentralized program and focus on these sorts of bigger picture issues.
In the rest of [the guide](##guide), we discuss design issues like this. For example,

+ Effectively using [automatic verification](##guide-assert) to check your application;
+ Fortifying your application against [non-participation](##guide-timeout);
+ Building [interaction abstractions](##guide-abstract) for related applications.


However, unless you're ready to dive deep now, the next steps for you are to:

+ [Install Reach](##install);
+ Work through the [tutorial](##tut);
+ Join <CommunityLink />.


Thanks for being part of Reach!

