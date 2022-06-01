# {#tut} Community insurance

This tutorial walks through the creation of a Reach decentralized application with React (v16.8 - Hooks) as the frontend.
It demonstrates how you can use Reach together with React to build a real-world community insurance application.
To follow along, you are expected to have already gone through [the most basic tutorial](https://docs.reach.sh/tut/) and you have already
learnt how to develop and test with a command-line frontend. 

We highly encourage you to read about [the Reach architecture](https://docs.reach.sh/rsh/#ref-programs) as well, 
as it's an excellent resource to help you understand the different modes/states of any Reach program.

## Assumptions
This tutorial assumes that you already
 * have [Docker](https://www.docker.com/get-started), and [Docker Compose](https://docs.docker.com/compose/install/) installed.
 * have node & npm installed on your machine. If not, first head over to the [official website](https://nodejs.org/en/download/) 
    and follow the installation steps.
 * have working knowledge of React hooks. You can checkout [the basics](https://reactjs.org/docs/hooks-overview.html#:~:text=Hooks%20are%20functions%20that%20let,if%20you'd%20like.) in case this is new to you.
 * installed Reach successfully by following the [installation process](https://docs.reach.sh/tut/) from the basic tutorial. 

Note that the complete code of the community insurance application we intend to learn how to build is already 
available on github and you don't have to re-write the entire code all over again. 
Simply clone the repository to your computer and use this tutorial to understand all the key parts of the code base.
I will explain the code line-by-line so be sure to follow this tutorial to the end. 

By the end of this tutorial, you will have enough confidence that you can even decide to re-develop it from scratch by yourself.
You will even have a bonus advantage to learn or to better understand some other concepts in React, Supabase and Tailwind CSS, 
so you will have comprehensive knowledge of how Reach fits together with these popular frontend technologies.

Since this is a real-world application, we want to style it professionally using [Tailwind CSS](https://tailwindcss.com/). 
You don't have to be familiar with this framework to follow along, though. 
The code you have cloned already has it all well configured. 
If you want to learn how to configure Tailwind CSS by yourself from scratch, 
checkout this documentation on [how to configure it with React](https://tailwindcss.com/docs/guides/create-react-app)

In case you want to first have some background knowledge on the insurance business, 
checkout [this pdf doc](https://ira.go.ug/cp/uploads/English%20Handbook%20final.pdf).

## Preparation
* Complete the most [basic tutorial](https://docs.reach.sh/tut/), finish the installation of Reach and all related configuration such as Docker.
* Clone the reference code from [my github repository](https://github.com/Reach-Insurance/react) to your local machine 
by running the following command.
Run these commands to initialise the project
```cmd
$ git clone https://github.com/Reach-Insurance/react.git
```
In case you get an error such as "git: command not found", then you need to 
first [install git](https://github.com/git-guides/install-git#:~:text=To%20install%20Git%2C%20navigate%20to,installation%20by%20typing%3A%20git%20version%20.).
If you are using windows, I recommend installing [git bash](https://www.atlassian.com/git/tutorials/git-bash) to your machine


At the root of your application code, there is a `package.json` file that contains all of the configuration for React 
and it is responsible for keeping track of all of the dependencies required to run this application.
If you open it, you will find there code that looks like this below.

```
load: /examples/insurance/package.json
```

Now open your terminal at the root of the cloned code directory and run the command below. 
Wait for `npm` to finish installing all React dependencies.
```cmd
$ npm install
```
## The Reach code and compilation outputs
If you had successfully completed [the basic tutorial](https://docs.reach.sh/tut/), by now you know that at the root of 
the project where there is a file named `index.rsh`, there should be a `reach` executable script which allows you to 
run commands like `reach compile` and `reach run`. However, the code you cloned does not have the `reach` executable script 
because the Reach code was already compiled and the output file `main.mjs` is already placed in the right place inside the code.
It is located inside the `src/reach-build/` directory. In case you want to compile the Reach code again, you need to include 
the executable script. By default `./reach compile` command places the output file(s) in a `build` folder at the root of the application,
but you can change the destination of the output by specifying the `-o` flag like so: 
```cmd
$ REACH_CONNECTOR_MODE="ALGO-browser" ./reach compile -o ./src/reach-build
```

## The Reach program (index.rsh file) for the Community Insurance DApp
```
load: /examples/insurance/index.rsh
range: 1-32
```
RECALL: Every reach program behaves like a state machine. It keeps changing "states", 
and in each state there are valid and invalid opearations. 
The states are called `steps` such as "step", "consensus step" and "local step".
In the code above, We have included comments to let you know when the program changes to a different state.

+ Lines 7 through 31 specify the participant who will deploy the contract, and acts as the Insurance provider. 
After deploying the contract, this "Insurer" participant will disconnect from it and leave it running for the 
community members to attach to it and interact with it to perform certain tasks such as paying monthly fees, 
raising insurance claims, approving claims, etc.

```
load: /examples/insurance/index.rsh
range: 32-60
```
+ Lines 33 through 52 specify the Community Member API. An API is similar to a class of participants that can 
be doing many different things with the contract at the  same time by calling the specified API functions. 
Please read more about APIs and [see how to define them](https://docs.reach.sh/rsh/appinit/#rsh_API) and 
be sure to get familiar with their syntax.
+ Lines 55 is a configuration which has to be in a "step" part of the code. 
You can find out [what this config is all about here](https://docs.reach.sh/rsh/appinit/#p_7).
+ Lines 57 is where the "step" state ends. 
Calling the init() terminates the initial step state and switches the program into a "consensus step" state/mode.

Now that we are in the consensus step mode, we can call any function that is valid for this step, such as `.only()`.
Also recall from the [reach program architecture](https://docs.reach.sh/rsh/#ref-programs) that calling any function 
may or may not switch the program state to another state. Forexample, calling `.only()` will switch the mode to "local step".

Let's contnue with writing our reach program code.
```
load: /examples/insurance/index.rsh
range: 56-74
```

+ Line 61 through 66 specifies the Insurer participant's local step operations.
+ On Line 63, the insurer declassifies the mandatory entry fee, which every community member registering for insurance services will have to pay.
+ On Line 64, the insurer declassifies a boolean which will tell the rest of the program to keep running until the deployer of the contract 
decides to terminate it. Initially this "contractIsRunning" variable is set to `true`. Later the deployer can decide to set it to `false`
by interacting with the contract again through some API function. This API function will be defined and explained later in this tutorial.
+ Line 65, enables the insurer to know that the contract has been successflly deployed, at the time of deployment.
+ On Line 68, the insurer publishes the 2 values that were declassified in the local step (Lines 63, 64).
An important note about the program states: On Line 67 we switched to consensus step because it is a "continuation" of `.only()` function. 
Now on Line 68 we are calling the `.publish()` function which if called from a consensus step keeps us in the consensus step still. 
Recall from the architecture that if this `publish()` function is called from a "step" then it changes the state of the program to "consensus step".
Therefore, we may conclude that this function always takes the program to a "consensus step".
+ On Line 69, we are setting the invariant condition to `true` initially. 
An invariant condition is a condition that evaluates to true before and after a while loop. We will need it later in our next piece of code.
You can read [more about invariant conditions](https://docs.reach.sh/rsh/consensus/#term_loop%20invariant).
+ On Line 70 we are calling the `commit()` function to terminate/end the continuation of `only()` which was called earlier. 
Remember, calling the `commit()` while in the consensus step changes the program state to "step" mode. 
The next steps in our program require us to be in a consensus step mode, but we are now in "step" mode after calling `commit()`.
+ On Line 71 we are calling `Insurer.publish()` in order to switch back to a consensus step.

```
load: /examples/insurance/index.rsh
range: 74-94
```
+ Line 75 defines a [Set](https://docs.reach.sh/rsh/consensus/#rsh_Set) of items called `registeredMembers`, which will keep a linear list of all community members knonwn to this 
insurance application. It only keeps their addresses, more info about the members is kept away from the blockchain (off-chain). 
+ Line 78 through 81 defines a [Map](https://docs.reach.sh/rsh/consensus/#rsh_Map) which will keep all open insurance claims. 
Once a claim is funded or has expired, it will be deleted from the list. 
+ Lines 85 through 92 define a Map to keep the owners of the open claims (ie, commucity members). 

Next, 
```
load: /examples/insurance/index.rsh
range: 94-251
```
In this part of the program we define all the operations that a community member can execute as they declared previously in the 
step part of the program where we specified the CommunityMember API.
Here we are coding the logic of the CommunityMember API functions one by one. 
Notice that we are using the [parallelReduce(...)](https://docs.reach.sh/rsh/consensus/#rsh_parallelReduce) function, which is one of the [consensus transfers](https://docs.reach.sh/guide/ctransfers/) in Reach. 
+ Lines 95 through 98 show the syntax of calling the parallelReduce function. 
Take note of the way it is expected to return an array of values which are saved in the variables on lines 96 and 97, 
and the initial values of these values are 1 and 1 respectively as they are the arguments passed to the `parallelReduce()` on line 98.
The array `[1, 1]` input into parallelReduce([1, 1]) match the left side of the call to this function `[membersCount, claimsCount]`. `membersCount`=1 and `claimsCount`=1.
Later these values will be changed by call to API functions in case a certain API function returns an 
array with different values from the these initial ones. 
If you have visited the sysntax of the [parallelReduce()](https://docs.reach.sh/rsh/consensus/#rsh_parallelReduce) function, then by now you know that
the general structure of the call to the `parallelReduce` is like this: 
```js
const [var1,var2, ...] = parallelReduce([initialVal1, initialVal2, ...]).api(x, x, x, x).api(x, x, x, x).timeout(...);
```
where `x, x, x, x` in the `.api()` function referes to the arguments of the api() which are also functions.
Those functions are the real logic of our application in regard to what community members will be doing with the application, the way they will interact with it.
We highly recommend reading more about the [syntax of the API.api()](https://docs.reach.sh/rsh/consensus/#rsh_parallelReduce.api) fucntion to get comfortable with 
what each of the argument functions should do. Be sure to understand each of the argument function in detail because this is the most important part you need to be able to effectively 
use API functions in general. It's worth taking your time on each of these functions as it will not leave you at the same level of understanding of 
APIs in Reach.

We are talking about this below (copied from the official [documentation here](https://docs.reach.sh/rsh/consensus/#rsh_parallelReduce.api)). 
```js
const LHS =                                         // LHS  refers to our left-hand-side [membersCount, claimsCount]
  parallelReduce(INIT_EXPR)                         // INIT_EXPR referes to our [1, 1] on line 98
  .define(() => DEFINE_BLOCK)                       // DEFINE_BLOCK refers to our code on Lines 99 - 111
  .invariant(INVARIANT_EXPR, ?INVARIANT_MSG)        // INVARIANT_EXPR referes to our "invariantCondition" = true
  .while(COND_EXPR)                                 // COND_EXPR refers to our "contractIsRunning" = true initially, changing it to false later will terminate the contract.
  .paySpec(TOKENS_EXPR)                             //we are not using .paySpec() in our program
  .case(PART_EXPR,                                  //we are not using .case() in our program
    CHECK_EXPR,
    PUBLISH_EXPR,
    PAY_EXPR,
    CONSENSUS_EXPR)
  .api(API_EXPR,                                    // API_EXPR refers to one of our API functions such as "CommunityMember.payMonthlyFee" on line 131
    ASSUME_EXPR,                                    // ASSUME_EXPR compares to our line 132 
    PAY_EXPR,                                       // PAY_EXPR compares to our Line 133 - (ob) => ob.mfee
    CONSENSUS_EXPR)                                 // CONSENSUS_EXPR refers to one of our functions on Lines 117-130, 134-140, 144-175, 179-213, 217-233 and 237-249
  .api_(API_EXPR,                                   // API_EXPR refers to one of our API functions such as "CommunityMember.createClaim" on line 141
    CHECKED_CONSENSUS_EXPR)
  .timeout(DELAY_EXPR, () =>                        //we are not using .timeout() in our program
    TIMEOUT_BLOCK);
```

If you have really followed through this tutorial step by step to this point, We are sure that you already feel confirtable 
working with APIs in Reach. Now let's write the last piece of code for our Reach program. 
It's similar to writing a conclusion statement while writing an essay in English human language. 
The only difference is that this (Reach) is not a human language, it's a programming language instead.
```
load: /examples/insurance/index.rsh
range: 251-267
```
+ Line 253 transfers any funds that remained on the "table" (left overs) to the deployer, the Insurer in this case.
+ Line 256 `commit()` function terminates the consensus step which we started on line 71 by calling `Insurer.publish()`
+ Finally Line 258, `exit()` ends the program. Once this line of code is executed, the deployed contract will exit from execution.
If we still want it to continue running, then we have to make sure we don't allow execution control to reach hear. 
Recall that we did that by using `contractIsRunning` boolean variable as our while loop condition, which can only become false 
when set explicitly in future. If we never set it to false explicitly, then the contract will never stop running.
+ Line 234 through 249 is responsible for stopping this contract once it has been deployed.

Our reach program is now complete and ready to communicate with a frontend. 


## The frontend in React, styled with Tailwind CSS
Next, let's write our React frontend code. 
In your `src` folder of the React app, find and open a file named `App.js`. Overwrite everything inside with the code below.
```
load: /examples/insurance/src/App.js
range: 1-18
```
Let's walk through the above code to understand what's going on. 
+ Line 1 imports React library and it's standard "Hooks" such as `useState`, `useEffect` and `useRef`.
+ Line 2 imports a custom CSS styling script `App.css`.
+ Line 3 imports the output of the compiled Reach program `./reach-build/index.main.mjs` as `backend`.
+ Line 4 imports `loadStdlib` from the standard Reach library `@reach-sh/stdlib`. It is later used on line 14 to construct a handle to the Reach standard library.
+ Line 5 imports `ALGO_WalletConnect` for connecting our frontend app to a deployed contract on Algorand network.
+ Line 6 imports a database client for `supabase`, which enables us to store some detailed information off-chain.
+ Line 7 imports a custom hook `useConfirm`, which allows us to create a custom dialog box for confirming certain actions before they can take effect.
+ On Lines 8 through 12, we import some custom components of our webpage, which were separated from the main `App.js` file to make the code more organised and readable. 
We could choose to have all of that code within the `App.js` file, but that would make it too long and difficult to read. 
Instead, we keep the code in separate smaller files and import them as React commponents. 
For example, `Dashboard`, `SignupForm`, `ErrorPage` and `Connect` are all components of the same webpage, our dashboard.
+ On line 14, we call the `loadStdlib` function to construct a standard library handle which will allow us to invoke Reach functions at the frontend. 
+ Lines 15 and 16 are the basic configuration needed to nstruct the handle.
+ Line 15 sets `REACH_CONNECTOR_MODE` to `ALGO-browser`, which means that the frontend should expect the Reach contract to be deployed on Algorand test network (testnet). 
+ Line 16 sets `PUBLIC_URL` to `https%3A%2F%2Fr.bridge.walletconnect.org`, which is the required public URL for WalletConnect to connect our app to the Algorand network.

```
load: /examples/insurance/src/App.js
range: 18-55
```
+ Line 19 calls the `reach.setWalletFallback` function, to set a provider fallback for users who may not have a wallet in their browser.
Read more about setting the [provider fallback](https://docs.reach.sh/frontend/#js_setWalletFallback).
+ Lines 20 through 27 has the basic configuration to set the fallback.
+ Line 28 specifies `WalletConnect` as the third party we would like to use to connect our app to the Algorand network. 
You can use an alternative to `WalletConnect` such as [MyAlgoConnect](https://wallet.myalgo.com/home). 
+ On line 31 through 33, we create a client of supabase database system where we will be storing all detailed data 
that would otherwise be too expensive to store on-chain. 
This client will enable us to perform database read, write, update and delete operations. For example, to insert into the database we will simply call the `insert` function like so: 
```js
const {data, error} = await supabaseClient.from("<table-name>").insert(<data>);
```
+ Line 35 is the beggining of the definition of `App` compnent, our main React component that contains the other smaller components such as `Dashboard`, `SignupForm`, among others.
+ Lines 36 through 55 defines all the React state variables for this particular `App` component. 
Each state variable is given its initial value by using one of the React hooks such as `useState` and `useRef`. 
+ Lines 55 through 146 all the definition of one special state variable named `interact`. 
```
load: /examples/insurance/src/App.js
range: 53-147
```
It is special in the sense of this application because it's the one that defines the different functions which were 
prevoiusly declared by the Reach program as the Insurer participant's valid opeartions.
We call the `useRef` React hook to set the `interact` state varible to its initial value as an object whose elements 
are the exact functions and properties declared by the Reach program's Insurer participant. 

+ On line 149, we call the `useEffect` React hook to define a piece of code that will be executed once when the `App` component mounts.
```
load: /examples/insurance/src/App.js
range: 147-170
```
+ Lines 151 through 166 define an `async` function `readFromDb()` which will run only once and 
try to read the info of a deployed contract from supabase. In case no info is found in the database, 
the state variable `deployed` will remain `false` as it was set initially, which will mean that the contract needs to be deployed first.
If the info is found, that means the contract is already deployed and therefore the current user is expected to be a community member 
and wants to attach to the contract. Line 158 sets the `deployed` state variable to true in order to later tell a deciding logic to treat 
the current user as a community member and to render the dashboard next instead of rendering the deployer page. 
The deployer page will be rendered in case the contract is not yet deployed. More about this logic will be explained later in this tutorial.

+ Lines 170 through 243 define 2 functions responsible for allowing a user to use one of 2 options to connect a wallet to the Community Insurance DApp.
+ On line 175, `ConnectWallet` will detect a wallet in the browser if the argument `loginWithMnemonic` is not explicitly set to `true`. 
+ Line 184 is a decision point. If `loginWithMnemonic` was explicitly set to true, we call `reach.newAccountFromMnemonic(mnemonicStr)` 
to create an account abstraction object by using the value of the input field that was entered by the user if they prefer to provide a 
mnemonic instead of detecting an existing wallet. Otherwise, a new account is created by detecting an existing. Either way, a promise is returned.
The returned promise resolves to an abstraction of a network user account.
+ On line 190, we call `.then()` on the promise that was returned on either line 185 or 187 and we pass a callback function which will then 
receive the `acc` object.
+ Line 192 `algoAccount.current = acc` sets the immutable state variable algoAccount to the created account object `acc`.
NOTE: In React hooks, whenever a state variable is declared and initialized by `useRef` hook, that state variable is immutable. 
That is to say, you can not directly access (read or update) its value. Its value can only be accessed through its `.current` property.
For example, `algoAccount = acc` is incorrecct; `algoAccount.current = acc` is correct.

+ On line 194, if the value of `deployed` state variable is `false`, then the current user is prompted to confirm whether they are the 
deployer and want deploy the contract. Recall that originally the moment this frontend app was launched, 
a piece of code that runs only once (Line 149 - 169) was executed to determined whether the contract is already deployed or not.
If contract info was found in supabase, the `deployed` variable was set to `true` or remained `false` by default, otherwise.
+ Line 197 creates a custom `confirm` prompt, the returned promise resolves to a boolean `true/false`. 
If true is returned, `wantToDeployContract` variable evaluates to `true` on line 199. 
+ Line 200 `setActivePage("DEPLOYER")` will set the `activePage` state variable to `true`, which will cause the `App` component to 
re-render with the deployer's page. By the way, our frontend app is a "single-page" application. "Pages" are rendered based on 
boolean state variables such as `activePage` in our case.
+ Line 202 sets an error message to be dsplayed in case the contract is not deployed and the current user does not want to deploy it.
+ Line 203 sets the link that a user will click to leave the error page.
+ Line 204 sets the activePage to the ERROR page so as to display the error set on line 202.
+ Line 208 redirects to the deployer page in case the contract is already deployed but "Deployer mode" is turned on. 
Deployer mode is a boolean variable which if set to true recognises the current user as a special 
community memeber with higher rights.
This is suposed to be the original deployer of the contract, but can no longer join the contract as an 
Insurer participant since he had previously disconnect by calling `withDisconnect()`. 
In case the deployer mode is not on, even the original deployer will be treated as a common community member.
+ Line 210 defines an async function which accesses the database to find out whether the current community member 
subscribed for insurance services or not. 
+ Line 225 creates a contract handle to attach to the already deployed contract. 
Notice that the contract info is read from the `contractInfo` immutable state variable, which was set previously (on line 159).
+ Line 227 sets the `activePage` state variable "DASHBOARD", which causes the `App` component to render the dashboard
 for a community member to perform tasks such as paying monthly fees, creating insurance claims, responding to claims, among others.
+ Line 229 will cause the `App` to render a signup page in case the boolean variable `isRegisteredMember` evaluates to false on line 223.
This means that the contract is already deployed but the current user is not yet a registered community member.
A registration form is rendered where this user provides their personal details and can then start to pay 
monthly fees to be considered for insurance services.

+ Lines 245 through 279 defines what happens when the contract is not yet deployed and the user that lauched the React application.
responds with a "Ok" when prompted to confirm if they want to deploy it.
```
load: /examples/insurance/src/App.js
range: 243-180
```
+ Line 245 starts the defnition of the `deployContract` function, which has the logic for deploying a contract and 
disconnecting from it as soon as it successfully gets deployed.

```
load: /examples/insurance/src/App.js
range: 280-288
```
Lines 282 through 287 defines the logic for stopping the contract. This is the code the will be executed if the deployer 
returns as a "special" community member by turning the deployer mode on. The backend has a check that authenticates 
whether the member trying to stop the contract is the original deployer. The check compares addresses.

```
load: /examples/insurance/src/App.js
range: 288-292
```
Lines 289 through 291 defines a function to control what hapens when a user refreshes the page.

Lines 293 through 359 is the return value from the main `App` omponent. 
```
load: /examples/insurance/src/App.js
range: 288-362
```
+ Line 295 checks if `activePage` state variable evaluates to "LOGIN", then the `App` component returns/renders the content of the imported `Connect` component.
+ Lines 298 through 306 is a list of inputs expected by the `Connect` component.
+ Line 309 checks if `activePage` state variable evaluates to "SIGNUP", then the `App` component returns/renders the content of the imported `SignupForm` component.
+ Lines 312 through 315 is a list of inputs expected by the `SignupForm` component.
+ Line 318 checks if `activePage` state variable evaluates to "DASHBOARD", then the `App` component returns/renders the content of the imported `Dashboard` component.
+ Lines 321 through 327 is a list of inputs expected by the `Dashboard` component.
+ Line 330 checks if `activePage` state variable evaluates to "DEPLOYER", then the `App` component returns/renders the content of the imported `Deployer` component.
+ Lines 333 through 341 is a list of inputs expected by the `Deployer` component.
+ Line 344 checks if `activePage` state variable evaluates to "ERROR", then the `App` component returns/renders the content of the imported `ErrorPage` component.
+ Lines 347 through 349 is a list of inputs expected by the `ErrorPage` component.

Finally line 362 exports the `App` compnent.

We are almost done.

Now that the main `App` component is complete, we only need to create all its utilities. 
That is, the subcomponents and other assets that support the main App compnent. 
These include images, custom hooks and sub-components such as Dashboard, SignupForm, etc.

Instead of taking a lot of time to create all these one by one, simply download them from 
our github repository and include them inside your `src` folder. 
The components' code is straight forward and easy to read and understand.
Head over to the link below and download 4 folders (components, hooks, store and images) with all their contents. 
[Download the components from github](https://github.com/reach-sh/reach-lang/examples/insurance/src/), 
include them in your `src` folder.

Now you can run the application by executing the command `reach react`.
That should display a login page where you can either enter a mnemonic or 
click a button to detect your existing wallet and connect to it.

Ensure your wallet app is set to use `testnet` and make sure the test account has some funds. 
You can fund your account or free by going to [Algorand Dispatcher](https://bank.testnet.algorand.network).

Supabase: The supabase credentials used in this tutorial point to our account test account, 
but you can create your own [free supabase account](https://supabase.com/docs/guides/api) and 
get your own credentials to use when creating a supabase client.






