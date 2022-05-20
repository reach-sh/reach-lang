# {#tut} Community insurance

This tutorial walks through the creation of a Reach decentralized application with React (v16.8 - Hooks) as the frontend.
It demonstrates how you can use reach together with react to build a real world community insurance application.
To follow along, you are expected to have already gone through [the most basic tutorial](##tut) and you have already
learnt how to develop and test with a command-line frontend. 
We highly encourage you to read about [the reach architecture](https://docs.reach.sh/rsh/#ref-programs) as well, 
as it's an excellent resource to help you understand the different modes/states of any Reach program.

## Assumptions
This tutorial assumes that you already
 * have [Docker](https://www.docker.com/get-started), and [Docker Compose](https://docs.docker.com/compose/install/) installed.
 * have node & npm installed on your machine. If not, first head over to the [official website](https://nodejs.org/en/download/) 
    and follow the installation steps.
 * have working knowledge of react hooks. You can checkout [the basics](https://reactjs.org/docs/hooks-overview.html#:~:text=Hooks%20are%20functions%20that%20let,if%20you'd%20like.) in case this is new to you.
 * installed Reach successfully by following the [installation process](##tut) from the basic tutorial. 

Since this is a real world application, we want to style it professionally using [tailwind css](https://tailwindcss.com/). You don't have to be familiar with this framework to follow along though. 
We'll provide a step by step configuration process to enable this styling framework.

In case you want to first have some background knowledge on the insurance business, checkout [this pdf doc](https://ira.go.ug/cp/uploads/English%20Handbook%20final.pdf).

## Preparation
* Complete the most [basic tutorial](##tut), finish the installation of Reach and all related configuration such as Docker.
* Initialize a react app (using [create-react-app](https://tailwindcss.com/docs/guides/create-react-app)) and configure tailwind css.

## Initialize the application
Go to the directory where you wish to create your project and open a new terminal from there.

Run these commands to initialise the project
```cmd
$ npx create-react-app community-insurance-dapp
$ cd community-insurance-dapp
```

To Install Tailwind CSS,
```cmd
$ npm install -D tailwindcss postcss autoprefixer
$ npx tailwindcss init -p
```

To Configure Tailwind CSS, open the file called `tailwind.config.js` at the root and paste in the following code.
```
load: /examples/insurance/tailwind.config.js
```
Open your `src/index.css` file and paste the code below, overwriting everything inside.
```
load: /examples/insurance/src/index.css
```

At the root of your application code, there a `package.json` file which contains all the configuration for react 
and it is responsible for keeping track of all the dependances required to run this application. 
Open it and paste the code bellow, overwriting everything inside.

```
load: /examples/insurance/package.json
```

Now run the command bellow and wait for npm to finish installing all React dependances
```cmd
$ npm install
```
You have now finished preparing your react frontend app. What about the Reach backend ? 
Well, if you had successfully completed [the basic tutorial](##tut), by now you know that at the root of 
the reach project there is a file named `index.rsh`, 
as well as the reach script file which allows you to run the command `./reach xxx`. 
To integrate both reach and react, all you need is to make sure that the output file generated after compiling the `.rsh` 
file resides inside the react project so that it can be imported at the top of react code. 
To achieve this, create a folder inside the src/ folder of the react app where the reach compilation output will go. 
While you can name it whatever you want, I suggest `reach-build`. So your reach compiled output should be place inside 
`community-insurance-dapp/src/reach-build`. By the way, you can direct the output to go there at the time of running your 
commpillation command. By default `./reach compile` command places the output in a `build` folder at the root of the reach app.
You can change the destination of the output by specifying the `-o` flag like so: 
```cmd
$ REACH_CONNECTOR_MODE="ALGO-browser" ./reach compile -o community-insurance-dapp/src/reach-build
```
The above command assumes that your `index.rsh` file is in the same directory with the `community-insurance-dapp` folder.
If that's not your case, you will have to adjest the command accordingly.

Preparation is done. Now lets start writing our Reach application code.

## Create your Reach program file for the community insurance dapp
```
load: /examples/insurance/index.rsh
range: range: 1-60
```
RECALL: Every reach program behaves like a state machine. It keeps changing "states", 
and in each state there are valid and invalid opearations. 
The states are called steps such as "step", "concensus -step" and "local-step".
In the code above, We have included comments to let you know when the program changes to a different state.

+ Lines 7 through 31 specify the participant who will deploy the contract, and acts as the Insurance provider. 
After deploying the contract, this "Insurer" participant will disconnect from it and leave it running for the 
community members to attach to it and interact with it to perform certain tasks such as paying monthly fees, 
raising insurance claims, approving claims, etc.
+ Lines 33 through 52 specify the Community Member API. An API is similar to a class of participants that can 
be doing many different things with the contract at the  same time by calling the specified API functions. 
Please read more about APIs and [see how to define them](https://docs.reach.sh/rsh/appinit/#rsh_API) and 
be sure to get familiar with their syntax.
+ Lines 55 is a configuration which has to be in a "step" part of the code. 
You can find out [what this config is all about here](https://docs.reach.sh/rsh/appinit/#p_7).
+ Lines 57 is where the "step" state ends. 
Calling the init() terminates the initial step state and switches the program into a "concensus-step" state/mode.

Now that we are in the concensus step mode, we can call any function that is valid for this step, such as `.only()`.
Also recall from the [reach program architecture](https://docs.reach.sh/rsh/#ref-programs) that calling any function 
may or may not switch the program state to another state. Forexample, calling `.only()` will switch the mode to "local-step".

Let's contnue with writing our reach program code.
```
load: /examples/insurance/index.rsh
range: range: 56-94
```

+ Line 61 through 66 specifies the Insurer participant's local step operations.
+ On Line 63, the insurer declassifies the mandatory entry fee, which every community member registering for insurance services will have to pay.
+ On Line 64, the insurer declassifies a boolean which will tell the rest of the program to keep running until the deployer of the contract 
decides to terminate it. Initially this "contractIsRunning" variable is set to `true`. Later the deployer can decide to set it to `false`
by interacting with the contract again through some API function. This API function will be defined and explained later in this tutorial.
+ Line 65, enables the insurer to know that the contract has been successflly deployed, at the time of deployment.
+ On Line 68, the insurer publishes the 2 values that were declassified in the local step (Lines 63, 64).
An important note about the program states: On Line 67 we switched to concensus step because it is a "continuation" of `.only()` function. 
Now on Line 68 we are calling the `.publish()` function which if called from a concensus step keeps us in the concensus step still. 
Recall from the architecture that if this `publish()` function is called from a "step" then it changes the state of the program to "concensus-step".
Therefore, we may conclude that this function always takes the program to a "concensus-step".
+ On Line 69, we are setting the invariant condition to `true` initially. 
An invariant condition is a condition that evaluates to true before and after a while loop. We will need it later in our next piece of code.
You can read [more about invariant conditions](https://docs.reach.sh/rsh/consensus/#term_loop%20invariant).
+ On Line 70 we are calling the `commit()` function to terminate/end the continuation of `only()` which was called earlier. 
Remember, calling the `commit()` while in the concensus-step changes the program state to "step" mode. 
The next steps in our program require us to be in a concensus-step mode, but we are now in "step" mode after calling `commit()`.
+ On Line 71 we are calling `Insurer.publish()` in order to switch back to a concensus-step.

+ Line 75 defines a [Set](https://docs.reach.sh/rsh/consensus/#rsh_Set) of items called `registeredMembers`, which will keep a linear list of all community members knonwn to this 
insurance application. It only keeps their addresses, more info about the members is kept away from the blockchain (off-chain). 
+ Line 78 through 81 defines a [Map](https://docs.reach.sh/rsh/consensus/#rsh_Map) which will keep all open insurance claims. 
Once a claim is funded or has expired, it will be deleted from the list. 
+ Lines 85 through 92 define a Map to keep the owners of the open claims (ie, commucity members). 

Next, 
```
load: /examples/insurance/index.rsh
range: range: 94-251
```
In this part of the program we define all the operations that a community member can execute as they declared previously in the 
step part of the program where we specified the CommunityMember API.
Here we are coding the logic of the CommunityMember API functions one by one. 
Notice that we are using the [parallelReduce(...)](https://docs.reach.sh/rsh/consensus/#rsh_parallelReduce) function, which is one of the [concensus transfers](https://docs.reach.sh/guide/ctransfers/) in Reach. 
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
We highly recommend reading more about the [syntax of the API.api()](https://docs.reach.sh/rsh/consensus/#rsh_parallelReduce.api) fucntion to get confirtable with 
what each of the argument functions should do. Be sure to understand each of the argument function in detail because this is the most important part you need to be able to effectively 
use API finctions in general. It's worth taking your time on each of these functions as it will not leave you at the same level of understanding of 
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
range: range: 251-267
```
+ Line 253 transfers any funds that remained on the "table" (left overs) to the deployer, the Insurer in this case.
+ Line 256 `commit()` function terminates the concensus-step which we started on line 71 by calling `Insurer.publish()`
+ Finally Line 258, `exit()` ends the program. Once this line of code is executed, the deployed contract will exit from execution.
If we still want it to continue running, then we have to make sure we don't allow execution control to reach hear. 
Recall that we did that by using `contractIsRunning` boolean variable as our while loop condition, which can only become false 
when set explicitly in future. If we never set it to false explicitly, then the contract will never stop running.
Lines 234 through 249 is responsible for stopping this contract once it has been deployed.

Our reach program is now complete and ready to communicate with a frontend. 

Next we write our frontend code in react. 





