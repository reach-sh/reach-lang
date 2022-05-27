# {#overview} Overview

This is an informal overview of Reach and the structure of a Reach program.
The goal of this document is to give enough technical specifics to help you understand what Reach does, but it isn't intended as either a [tutorial](##tut) or a [reference](##ref).
When you're ready to really begin a project, you can start with one of those, or [the workshop](##workshop).

If you have experience with blockchain development using existing tools, we recommend reading this article and [a comparison with other development platforms](##guide-solidity).

:::note
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

:::note
You can look at the entire example program by visiting [overview/index.rsh](@{REPO}/examples/overview/index.rsh).
:::

:::note
Get language support for Reach in your editor by visiting [the guide on editor support](##guide-editor-support).
:::

The main part of the program looks like this:

```
load: /examples/overview/index.rsh
md5: c3ba149f23c49dec516123979898b14b
range: 1-15
```

+ Line 1 specifies that this is a Reach program.
+ Line 2 specifies that this program will be compiled with strict mode, which enables unused variable checks.
+ Line 4 defines the main export from this program. `main` is the default used by Reach.
+ Line 4 also specifies that it is an application.
+ Line 5 specifies that the program identifier `{!rsh} A` will represent the Alice participant.
+ Lines 6 and 7 specify the interface between Alice's participant and frontend. In this case, Alice's frontend must provide a number called `request` and a string called `info`.
+ Line 9 specifies that the program identifier `{!rsh} B` will represent the Bob participant.
+ Lines 10 and 11 specify the interface for Bob, which includes a function named `want`, that takes a number and returns `{!rsh} null`, as well as a function named `got`, that receives the information.
+ Finally, line 13, deploys the DApp.

The elided lines, 14 through 34, contain the body of the application, which we can divide into four parts.

```
load: /examples/overview/index.rsh
md5: c3ba149f23c49dec516123979898b14b
range: 15-18
```

+ Lines 15 and 16 specify that Alice takes a local step where she declassifies the amount of tokens requested.
In Reach, all values from the frontend are secret until explicitly made public with declassify.
+ Line 17 has Alice join the application by publishing that value, and the logic of the program transitions to specifying what the contract does.
+ Line 18 has the contract commit to these values and continue the rest of the program.

At this point, Bob's backend has learned the value of `{!rsh} request` and can deliver it to Bob's frontend for his approval. This happens next.

```
load: /examples/overview/index.rsh
md5: c3ba149f23c49dec516123979898b14b
range: 20-23
```

+ Lines 20 and 21 have Bob perform that delivery.
`{!rsh} interact.want` doesn't explicitly return a boolean because the frontend cannot return if Bob doesn't want to continue.
A better version of this program might return `{!rsh} false` and have that communicated to Alice.
+ Lines 22 and 23 have Bob join the application and submit a payment matching the appropriate amount, and then the contract commits.

It's now Alice's turn again:

```
load: /examples/overview/index.rsh
md5: c3ba149f23c49dec516123979898b14b
range: 25-29
```

+ Lines 25 and 26 specify that Alice declassifies the information.
+ Line 27 has her publish it.
+ Line 28 has the contract transfer the requested amount to her.
+ Line 29 commits the transactions on the consensus network.

The only thing left is for Bob's backend to deliver the information to his frontend.

```
load: /examples/overview/index.rsh
md5: c3ba149f23c49dec516123979898b14b
range: 31-33
```

+ Line 31 and 32 use an interaction expression to transfer the information to the frontend.
+ Line 33 exits the program.

---

Reach programmers don't need to think about details like _contract storage_, _protocol diagrams_, _state validation_, or _network details_; instead, they can focus exclusively on the business logic of their application.

## {#over-compile} Compile

After a Reach programmer writes this application in a file like [`overview/index.rsh`](@{REPO}/examples/overview/index.rsh), they could run

```cmd
$ reach compile overview/index.rsh
```

and the `build` directory will contain a new file named [`index.main.mjs`](@{REPO}/examples/overview/build/index.main.mjs), which contains a JavaScript implementation of a backend for each participant, as well as the Ethereum bytecode for the contract.

:::note
If you are curious, you can take a look at this file by going to [overview/build/index.main.mjs](@{REPO}/examples/overview/build/index.main.mjs).
The Ethereum bytecode is not readable, but if you understand Solidity, you may want to look at [overview/build/index.main.sol](@{REPO}/examples/overview/build/index.main.sol) to see the original Solidity source that it is compiled from.
Reach can leave files like these in place when run with `--intermediate-files`.
:::

:::note
The command line snippets in this overview make no assumption about where Reach is installed on your machine.
If Reach is installed in the directory you're working in, point to it with `./reach`; if it is in a parent directory use `../reach`; if you installed it into your `{!cmd} PATH`, just type `reach`.
Learn more about installing Reach in the [Tools documentation](##ref-install) or in our [Tutorial](##tut).
:::

For this thirty line application, the Reach compiler generated hundreds of lines of JavaScript code in two functions, one for Alice and one for Bob.
Separately, it generated hundreds more lines of Solidity code to implement the contract.
If a programmer wasn't using Reach, they would have to write all this code in these three modules individually and keep them synchronized at every step of the development process.

Moreover, Reach doesn't only work for Ethereum: it is blockchain agnostic and can be easily configured to use a different connector to target a different [consensus network](##ref-networks), like Algorand.
Nor is Reach tied to JavaScript: it can be configured to target other [backend languages](##ref-backends-rpc), like Go, Python, and C#.

## {#over-verify} Verify

Reach doesn't just compile your program: it also verifies it and ensures that entire categories of errors don't occur.
For example, it always guarantees that the balance in the contract at the end of the program is zero.
This is important because if it were not true, then tokens would be locked away by the contract and inaccessible.

For this example program, it is obvious that when a single transfer of `{!rsh} request` goes in at line 22 and a single transfer of `{!rsh} request` goes out at line 28, then the balance is zero at the end of the program.
We could make a small tweak, however, to demonstrate things going wrong.

Let's change the third step to leave a single unit in the balance:

```
load: /examples/overview/index-error.rsh
md5: 0756828e5a4ce18ee4835063706594ae
range: 25-29
```

And then run the compiler:

```cmd
$ reach compile overview/index-error.rsh
```

It will print out a detailed error message showing the violation.

```
load: /examples/overview/index-error.txt
md5: 7e916074ca72431b3520b6fc1996c8df
range: 2-28
```

Verification failures include a lot of information, such as a concrete counter-example showing values that could have been provided by frontends that would lead to the property failing to hold.
In this case, it reports that if Alice were to pass an `{!rsh} interact.request` over `{!rsh} 1` at the start of the program on line 5, then the balance of the contract would not be provably `{!rsh} 0` at the end of the program.

---

Reach programmers don't need to worry about entire categories of errors because the compiler automatically checks their code and ensures that those errors aren't present.
Of course, there's a lot more to say about the details of [automatic verification](##guide-assert); indeed, it is one of the most powerful features of Reach, but we'll leave it at that for now.

## {#over-interface} Interface

The backend produced by the Reach compiler isn't an application on its own.
In particular, each participant needs a frontend to interact with.
In a real deployment, this interfacing code would be tied to a GUI, like a Web or smartphone app.
Let's look at a simple command-line version that demonstrates how it would work for testing on a private devnet.

:::note
You can look at the entire example interface program by visiting [overview/index.mjs](@{REPO}/examples/overview/index.mjs).
:::

The program is just a few dozen lines long and the shell of it is quite simple:

```
load: /examples/overview/index.mjs
md5: b0ab403c2babb38238d85d34b318316a
```

+ Lines 1 and 2 import the Reach standard library loader and the compiled app backend.
+ Line 4 dynamically loads the appropriate network-specific Reach standard library,
based on the `REACH_CONNECTOR_MODE` environment variable.
All of Reach's network-specific standard libraries adhere to a common interface allowing you to write programs that are network-agnostic.
+ Lines 6 and 7 initialize new test accounts for Alice and Bob.
+ Line 9 has Alice deploy the contract on the consensus network.
+ Line 10 has Bob attach to the contract.
The value `{!js} ctcAlice` contains no secret information and could easily be printed out and shared with Bob outside of the consensus network.
+ Lines 12 through 21 launch the backends and wait for their completion. We'll look at the details in a moment.

This code, similar for all test programs, demonstrates how straightforward it is to scaffold a Reach application for testing.

Let's look at initializing and interfacing each participant, starting with Alice.

```
load: /examples/overview/index.mjs
md5: b0ab403c2babb38238d85d34b318316a
range: 13-16
```

+ Line 13 uses the contract handle associated with Alice's account to run the Alice participant backend, passing an object which holds the interact functions.
+ Line 14 provides the `{!rsh} request` value.
+ Line 15 provides the `{!rsh} info` value.

Let's look at Bob next.

```
load: /examples/overview/index.mjs
md5: b0ab403c2babb38238d85d34b318316a
range: 17-20
```

+ Line 17 initializes Bob just like Alice, although we use the `{!js} p` short-hand.
+ Line 18 provides his `{!rsh} want` function, which produces a log message and always accepts.
+ Line 19 provides his `{!rsh} got` function, which displays the secret on the console as well.

---

Reach completely abstracts all the details of the chosen consensus network from the programmer, except for those directly impinging on business decisions, like the amounts of currency transacted.
Reach allows programmers to focus on the business logic of their application at every stage, from the core application to the interfacing elements.

## {#over-execute} Execute

It's now time to execute this test program and ensure that everything is working correctly.
In this case, we've set up our application simply: there's one Reach file for the application and one JavaScript file for the interface.
This is a common practice, so Reach comes with a simple wrapper script to build and execute such applications.

First, we connect Reach to a consensus network. We can connect to Ethereum's test environment by running:

```cmd
$ export REACH_CONNECTOR_MODE=ETH
```

Then, we just run:

```cmd
$ reach run
```

And then Reach

+ compiles [overview/index.rsh](@{REPO}/examples/overview/index.rsh);
+ creates a temporary Node.js package;
+ builds a Docker image based on Reach's standard image for the package; and,
+ runs the application connected to the specified consensus network's devnet image.

On typical developer laptops, this entire process takes seconds and can be completely integrated into existing development [IDEs](##guide-editor-support), like VSCode, so Reach developers can compile, verify, build, launch, and test their Reach app with a single command.

---

Reach completely abstracts all the details of building and maintaining consensus network test environments and build scripts from the programmer, so they can focus exclusively on the business logic of their application.
In fact, Reach works for multiple networks, so if we instead run

```cmd
$ REACH_CONNECTOR_MODE=ALGO reach run
```

then Reach will start up a private Algorand devnet and use the Algorand connector.
The developer does not need to change anything about their program because Reach is entirely agnostic to the consensus network choice during deployment.

The same goes for Conflux:

```cmd
$ REACH_CONNECTOR_MODE=CFX reach run
```

## {#over-react} Web app

:::note
You can [watch a 7-minute video](https://www.youtube.com/watch?v=jHEKIMNvs-o) on YouTube
which demonstrates this section's code in action
and provides a brief explanation of how it works.
:::

The previous section uses Node.js to perform a test run at the command line.
However, most Reach developers deploy their DApps via a Web application, as we describe below.

A Web deployment uses the exact same [index.rsh](@{REPO}/examples/overview-react/index.rsh) file connected, this time, to a React-based [index.js](@{REPO}/examples/overview-react/index.js) file.
(It also uses some simple React [views](@{REPO}/examples/overview-react/views)
 and [`css`](@{REPO}/examples/overview-react/index.css) to go with it.)
Let's take a look at some snippets from the React [index.js](@{REPO}/examples/overview-react/index.js) and compare with the Node.js [index.mjs](@{REPO}/examples/overview/index.mjs) from before:

```
load: /examples/overview-react/index.js
md5: 6045f1f418f5096943132afe8db5bbe0
range: 7-9
```

At the top of the file, we import the Reach-generated backend as `{!js} backend` and we load the standard library as `{!js} reach`.

```
load: /examples/overview-react/index.js
md5: 6045f1f418f5096943132afe8db5bbe0
range: 27-28
```

We hook into the App [component](https://reactjs.org/docs/react-component.html)'s [lifecycle event](https://projects.wojtekmaj.pl/react-lifecycle-methods-diagram/) `{!js} componentDidMount`
in order to fetch the user's account.
`{!js} getDefaultAccount` automatically interacts with browser extensions, like MetaMask, to get the user's
currently-selected account.
Reach is able to deploy contracts and send transactions to the consensus network by prompting the user directly through the extension's API, without additional assistance from the React frontend.
This is just like how in the Node.js deployment, the Reach programmer does not need to decode the details of the underlying consensus network's interaction API.

```
load: /examples/overview-react/index.js
md5: 6045f1f418f5096943132afe8db5bbe0
range: 71-76
```

Our React component has a method called `{!js} deploy` that actually deploys the contract on the network, using the same calls as in the test deployment:
on line 72 we call the `{!js} acc.deploy` function,
and on line 74, we call the `{!js} ctc.getInfo` function;
exactly as we did for the Node.js program.

```
load: /examples/overview-react/index.js
md5: 6045f1f418f5096943132afe8db5bbe0
range: 79-85
```

Similarly, we implement a `{!js} runBackend` method that executes the Reach program as Alice using information gathered from the React UI.

```
load: /examples/overview-react/index.js
md5: 6045f1f418f5096943132afe8db5bbe0
range: 112-121
```

We implement a similar method in the `{!js} Bob` component that runs the backend as Bob.

We specify Alice's and Bob's respective participant interact interfaces
just as we would in Node.js.
In the React program,
we have the ability to leverage Bob's `{!rsh} interact` functions as callbacks
that can update the React state
in order to display to, or harvest information from, the React user interface.

You can install the `@reachsh/stdlib` JavaScript library
into your React project,
or for convenience, instead of setting up a React project,
you can simply use the command

```cmd
$ reach react
```

This command runs your DApp with the React development server in a Docker container which has Reach and React JavaScript dependencies pre-installed, so it starts up much faster than building them yourself.

As before, you can use `REACH_CONNECTOR_MODE` to  choose your desired connector.

```cmd
$ REACH_CONNECTOR_MODE=ETH reach react
```

```cmd
$ REACH_CONNECTOR_MODE=ALGO reach react
```

```cmd
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

+ [Install Reach](##ref-install);
+ Work through the [tutorial](##tut);
+ Join [the Discord community](@{DISCORD}).

Thanks for being part of Reach!
