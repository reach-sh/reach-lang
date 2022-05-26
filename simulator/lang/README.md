# SimLang V0.0.49

The Reach Programmatic Simulator is implemented as a TypeScript library.

It is available on [npm](https://www.npmjs.com/package/@reach-sh/simulator-lang).

```javascript
"name": "@reach-sh/simulator-lang",
  "description": "language/library for the Reach Simulator",
  "version": "0.0.49",
  "main": "dist/index.js",
  "types": "dist/index.d.ts",
  "repository": {
    "type": "git",
    "url": "git+https://github.com/reach-sh/reach-lang.git",
    "directory": "simulator"
  }
```

# Preface: Design

Reach does not support a traditional interpreter. Reach programs can only be run by first implementing a frontend and then simulating that frontend, which embeds interaction with the Reach program. This is by design because Reach depends on frontends and a consensus network to operate.

However, we have designed a symbolic simulator that operates without either frontend or consensus network. It works by directly executing the linear Reach intermediate language and presents a UI where users can interactively explore a trace of an execution. They specify a linearization of the history of many participants interacting with the Reach program, including the participants coming into existence and attaching.

At every branching point (such as when multiple backends are awaiting interactive decisions from their frontends or when two participants are racing to publish a value), the user can make a choice and explore the resulting behavior. The goal of the simulator is to help Reach programmers better understand the numerous possibilities regarding how their programs may execute under certain variable conditions.

This interactive simulator will also, in the future, be integrated with our theorem prover so value choices can be left abstract and only concretized when future choices restrict them. This will mean that we can execute and explore unverified Reach programs and present the results of verification in the same framework as users can experience when manually exploring the behavior of their program.

While the Reach Simulator can be used with a graphical web-based UI, this document focuses on a textual language to specify simulation explorations (SimLang) so they can be iterated throughout development.

# Introduction

The Reach Simulator is a framework for running/experimenting with decentralized applications using an artificial consensus network, and an interactively created synthetic frontend specified by the user as the program executes.

The Reach Programmatic Simulator (SimLang) is designed to be a testing/debugging/pedagogical tool for new and existing Reach programmers.

Because there is no need to deal with the specifics of various actual consensus networks and frontends when using the simulator, the mental effort of executing a Reach program is reduced.

This aids learning of Reach and DApp concepts/semantics in a simplified environment.

Additionally, the Reach Simulator is designed to allow users to interactively explore the state of the program in various ways, in order to better enable them to understand the semantics and behavior of their program on the blockchain, and how the actions of various actors and the Consensus may affect global and local program states.

# Features/Documentation
This section specifies the relevant objects which are exposed by the SimLang, as well as their attributes, methods and corresponding types.

## Scenarios
Scenarios are intended for management and organization of testing environments. They help with bookkeeping of program state.

```javascript
// parent class
class Scenario {
  top: State;
  state: State;
  participants: Record<string, Participant>;
  consensus: Consensus;
  apis: Record<string, API>;
  views: Record<string, View>;

  constructor(): Scenario

  // set up
  async init(): Scenario

  // ping the server for a friendly greeting
  async pingServer(): string

  // reset the server
  async reset(): null

  // list program states
  async programHistory(): number[]

  // current actor
  async getCurrentActor(): Actor

  // new account
  async newTestAccount(): Account

  // new token
  async launchToken(): Token

  // set participant's scenario
  who(part: Participant): Participant

  // artificially pass time
  // this will not cause a timeout
  async wait(n: number): Scenario

  // artificially cause timeout
  async forceTimeout(): Scenario

}

```

## FunctionalScenarios
As the simulation proceeds, information is appended to the program state. The _FunctionalScenario_ tracks these additions functionally: producing a new _Scenario_ for each addition.

```javascript
class FunctionalScenario extends Scenario {

  constructor(): FunctionalScenario

}
```

## ImperativeScenarios
As the simulation proceeds, information is appended to the program state. The _ImperativeScenario_ tracks these additions imperatively: modifying the existing _Scenario_ for each addition.

```javascript

class ImperativeScenario extends Scenario {

  constructor(): ImperativeScenario

  copy(): ImperativeScenario

}
```

## Actors

```javascript
class Actor {
  id: number;
  account: Account;
  name: string;
  scene: Scenario;

  constructor(id: number,account: Account,name: string,scene: Scenario): Actor

  // next action
  async getNextAction(): Action

  // local store
  async getStore(): Store

  // named variable
  async getVar(v:any): Variable

  // token-balance mapping
  async getWallet()

  // balance for token
  async balanceOf(tok: Token = nwToken): number

  // current consensus phase
  async getPhase(): number

  // current states
  async getStatus(): string

  // state history
  async history(): number[]

}
```

## Actions

```javascript
class Action {
  id: number;
  name: string;
  owner: Actor;
  scene: Scenario;
  contents: any;

  constructor(id: number,name: string,owner: Actor,scene: Scenario, contents: any): Action

  // resolve action
  async resolve(resp: any = -999,ty: string = "number")
}

```

## Participants

```javascript
class Participant extends Actor {
  id!: number;
  account!: Account;
  name!: string;
  scene!: Scenario;

  constructor(id: number,account: Account,name: string,scene: Scenario): Participant

  // set up participant
  async init(blce="",liv={},accID=""): [Scenario, Participant]

  // perform next interact
  // this will also perform any receives before this interact
  // throws error upon failure
  async interact(name:string,val:any): Scenario

  // attempt to exit app
  // this will also perform any receives before exit
  // throws error upon failure
  async exit(): Scenario

  // perform next receive action
  // throws error upon failure
  async receive(): Scenario

}

```

## Consensus

```javascript
class Consensus extends Actor {
  id!: number;
  account!: Account;
  scene!: Scenario;

  constructor(account: Account,scene: Scenario): Consensus

  // perform the next publication
  async publish(ac:Participant): Scenario

  // perform a currency transfer
  async transfer(s: number,fr: Actor,to: Actor,tok: Token,amt: number)

  // get the full ledger
  async getLedger()

  // get the full map (linear) state
  async getMapState()

  // get network time
  async getNetworkTime(): number

  // get network seconds
  async getNetworkSeconds(): number

  // get program log
  async getLog()

}

```

## Variable

```javascript
class Variable {
  v: any;

  constructor(v: any)

  assertVar(t: string,v: any)

  contents(): any

}
```

## APIs

```javascript
class API {
  id: number;
  name: string;
  scene: Scenario;

  constructor(id: number, name: string, scene: Scenario): API

  // perform an API call
  async call(v: any,t: string)
}
```

## Views

```javascript

class View {
  id: number;
  name: string;
  vari: string;
  tag: string;
  contents: string;
  scene: Scenario;

  constructor(id: number, name: string, vari: string, tag: string, contents: string, scene: Scenario): View

  // perform a view call
  async call(v: any,t: string)
}
```

# Tutorial
The SimLang tutorials build off the Reach Language tutorials. Below we discuss example simulations.

## rps-2-rps

Firstly we import the SimLang module, as well as the `assert` module for testing equality.

```javascript
import * as lang from '@reach-sh/simulator-lang';
import * as assert from 'assert';
```

For any simulation, we may choose to use either the `FunctionalScenario` or the `ImperativeScenario`, the decision is a matter of programming style preference.

The imperative style mutates the testing environment as the simulation proceeds, while the functional style produces an entirely new testing environment at every step of the simulation, which can be useful for intuitive _state branching_ and testing multiple possible outcomes in a single test script run.

The imperative style uses an explicit `copy` function to enable branching behavior.

```javascript
const is = new lang.ImperativeScenario();
```

Scenarios need an `init` function in addition to the constructor, in order to perform `async` loading of the test environment data.

```javascript
await is.init();
```

Prior to running this JavaScript code, we started the SimServer with the command `reach compile --simulate`. We can ping the server to confirm that it is up and running.

```javascript
const pi = await is.pingServer();
```

The `Scenario` has information about the participants in the current simulation.

```javascript
const alice = is.participants.Alice;
const bob = is.participants.Bob;
```

As well as the consensus.

```javascript
const consensus = is.consensus;
```

Participants need an `init` function in addition to the constructor, in order to perform `async` loading of the test participant data.

```javascript
const [, a] = await alice.init();
const [s, b] = await bob.init();
```

After designing and building a multiplayer game such as _Rock, Paper, Scissors_, a straightforward approach to testing the game comprehensively might involve running the game with all possible inputs from each player and asserting the outcomes. SimLang makes this process easy and convenient by allowing us to _branch state_ at this point in the simulation and run the game for all possible inputs using the `play` function as seen below. `play` returns the actual outcome of the game, which we then compare against our expected outcome.

```javascript
for (let aHand = 0; aHand < 3; aHand++) {
  for (let bHand = 0; bHand < 3; bHand++) {
    const r = await play(s.copy(),aHand,bHand,a,b,consensus);
    r.assertVar(winner(aHand,bHand));
  }
}
```

The `play` function is defined as follows. It accepts:

1. a reference to a Scenario `sc`
2. Alice's input/hand `aHand`
3. Bob's input/hand `bHand`
4. a reference to the participant `alice`
5. a reference to the participant `bob`
6. a reference to the  `consensus`

In this example, we are simulating the most basic form of the game _Rock, Paper, Scissors_. Alice plays her hand, and it is published by the consensus. Then Bob plays his hand, and it is published by the consensus. Both players learn the outcome of the game, and subsequently exit the decentralized application.

```javascript
const play = async (sc,aHand,bHand,alice,bob,consensus) => {
  let s = await sc.who(alice).interact('getHand', aHand);
  s = await s.who(consensus).publish(alice);
  s = await s.who(bob).interact('getHand', bHand);
  await consensus.publish(bob);
  await alice.exit();
  await bob.exit();
  const r = await alice.getStatus();
  // game over
  assert.equal(r,"Done");
  return alice.getVar('outcome');
}
```

## rps-6-timeouts
In this section of the tutorial, naturally we are inclined to demonstrate exploring timeouts with SimLang. For convenience, rather than having to think about the clock when attempting to test timeout situations, the SimLang library provides a `forceTimout` method with the `Scenario` object.

```javascript
// imports
import * as lang from '@reach-sh/simulator-lang';
import * as assert from 'assert';

const main = async () => {
  console.log("Init Testing!")

  // functional scenario this time
  const fs = new lang.FunctionalScenario();
  let s = await fs.init();
  let a;
  let b;
  const pi = await fs.pingServer();
  const alice = fs.participants.Alice;
  const bob = fs.participants.Bob;
  const consensus = fs.consensus;

  // init alice with a balance of 10
  // also the init values: wager, deadline
  [s, a] = await s.who(alice).init(10,
    {'wager': new lang.ReachNumber(10).format(),
    'deadline': new lang.ReachNumber(99).format()}
  );
  ```

We see here that the `Participant` `init` function returns a list of two values: the new `Scenario` where that `Participant` is now running the DApp, and a reference to the initialized `Participant` which will now include more information such as a `Wallet` and a local `Store`.

__Note__: The `consensus` `Actor` is initialized automatically by the Simulator, because it never has any init values.

__Note__:  `Actor` is the `Participant` superclass. In the Reach Simulator, only the `consensus` object is an `Actor` but not a `Participant`.

__Note__:  When simulating with the the `ImperativeScenario`, the `Participant` `init` function still returns a reference to the new `Scenario`, but since we're tracking it by reference this value can be ignored.

```javascript
  [s, b] = await s.who(bob).init(10);

  // we define a play function
  const play = async (s,aHand,bHand,alice,bob,consensus) => {
    // Alice interactively gets her hand (0)
    s = await s.who(alice).interact('getHand', aHand);
    // getRandom
    s = await s.who(alice).interact('random', 4444);
    // Alice's wager/deadline is published
    s = await s.who(consensus).publish(alice);
    // Alice receives the results of publication
    s = await s.who(alice).receive();
    // ...
```

The reader may have noticed that, unlike in the `ImperativeScenario` which modified its own object reference at every step of the simulation, with the `FunctionalScenario` we must modify the reference `s` ourselves. This presents us with the opportunity to, at any point, give the scenario reference a unique name, essentially transforming it into a simulator _breakpoint_.

```javascript
	// ...
    // Bob interactively gets his hand (1)
    // and also receives Alice's publication
    // let's name a special "breakpoint" that we'll return to
    // in order to test different timeout scenarios
    //  ↓↓
    let sBeforeTimeout = await s.who(bob).interact('getHand', bHand);

    // force Bob's hand publish to timeout!
    s = await sBeforeTimeout.forceTimeout();

    // timeout
    s = await s.who(consensus).publish(bob);
    s = await s.who(alice).receive();
    s = await s.who(bob).receive();
    // closeTo
    s = await s.who(consensus).publish(alice);
    s = await s.who(alice).receive();
    s = await s.who(bob).receive();
    // first scenario done
    let w = await alice.balanceOf();
    // check that Alice kept her money
    assert.equal(w,10);

    // test the scenario where Alice times out
    // we're going back in time to our breakpoint here
    //        ↓↓
    s = await sBeforeTimeout.who(consensus).publish(bob);
    s = await s.who(alice).receive();
    s = await s.who(bob).receive();

    // force Alice's hand publish to timeout!
    s = await s.forceTimeout();
    // timeout
    s = await s.who(consensus).publish(alice);
    s = await s.who(alice).receive();
    s = await s.who(bob).receive();
    // closeTo
    s = await s.who(consensus).publish(bob);
    s = await s.who(alice).receive();
    s = await s.who(bob).receive();
    w = await bob.balanceOf();
    // check that Bob got everything
    assert.equal(w,20);
  }
  // play game with these inputs/assertions
  await play(s,0,1,a,b,consensus);
  console.log("Testing Complete!!!")
}

main();
```

## rps-7-loops

In order to demonstrate the power of the simulator, we will go about testing the `rps-7-loops` tutorial as follows: we simulate the beginning of the program normally, but upon reaching the while loop which tests the `DRAW` condition, rather than running through the full loop naturally, we repeatedly re-run the original iteration of the loop, restarting the loop each time with random inputs, until someone actually wins.

```javascript
import * as lang from '@reach-sh/simulator-lang';
import * as assert from 'assert';

const main = async () => {
  console.log("Init Testing!")
  const DRAW = 1;
  const fs = new lang.FunctionalScenario();
  let s = await fs.init();
  let alice; let bob;
  const a = fs.participants.Alice;
  const b = fs.participants.Bob;
  const consensus = fs.consensus;
  [s, alice] = await s.who(a).init(10,
    {'wager': new lang.ReachNumber(10).format(),
    'deadline': new lang.ReachNumber(99).format()}
  );
  [s, bob] = await s.who(b).init(10);
  s = await s.who(consensus).publish(alice);
  s = await s.who(bob).receive();
  s = await s.who(consensus).publish(bob);

  // here we define the play function to represent an
  // iteration of the while loop
  const play = async (s,alice,bob,consensus) => {
    let aHand = Math.floor(Math.random() * 3);
    let bHand = Math.floor(Math.random() * 3);

    s = await s.who(alice).interact('getHand', aHand);
    s = await s.who(alice).interact('random', (Math.floor(Math.random() * 4444)));
    s = await s.who(consensus).publish(alice);

    s = await s.who(alice).receive();
    s = await s.who(bob).interact('getHand', bHand);
    s = await s.who(consensus).publish(bob);
    s = await s.who(alice).receive();
    s = await s.who(bob).receive();

    s = await s.who(consensus).publish(alice);

    let outcome = (await s.who(consensus).getVar('outcome')).contents();
    // when using the FunctionalScenario, helper functions need to return
    // the final scenario reference
    //     ↓↓
    return [s,outcome];
  }

  let outcome = DRAW;
  let counter = 0;
  while (outcome === DRAW) {
    counter++;
    [s,outcome] = await play(s,alice,bob,consensus);
  }

  s = await s.who(alice).exit();
  s = await s.who(bob).exit();
  // show the game outcome, which is guaranteed to not
  // be a draw
  console.log(outcome);
  // show how many times we had to play to find
  // a clear winner
  console.log(`game played ${counter} times`);
  console.log("Testing Complete!!!");
}

main();

```

# Conclusion

The Reach Programmatic Simulator (SimLang) is a powerful and flexible tool that allows developers to debug and create exhaustive tests for their Reach programs.

These tests are connector-agnostic and do not require any specific test network to execute.

The simulator snapshots/preserves the entire history of blockchain information for inspection: including the `Global` ledger, and the `Local` `Participant` information.
