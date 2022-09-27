# {#morra} MORRA!

This tutorial is for complete beginner, it assumes no prior experience with Dapp or Blockchain.

It walks through the creation of a simple Morra game with Reach. This game would be deployed to Reach-provided developer testing network. We will also create a frontend to interact with it. You can checkout the final version of the application [here](https://morra-game.vercel.app/).

If this is too simple, then you may want to check other tutorials or start [the workshop](##workshop) for larger and less constrained projects or [the reference manual](##ref) for the minute details of Reach.

Before we start coding, let's make sure you have everything you need to start creating Dapp with Reach!

## {#morra-0} Install and Initialize

To install reach, you  need to have these three installed:
- [make](https://en.wikipedia.org/wiki/Make_(software))
- [Docker](https://www.docker.com/get-started)
- [Docker Compose](https://docs.docker.com/compose/install/)

Let's confirm that you have them installed:

```bash
$ make --version
```

```bash
$ docker --version
```

```bash
$ docker-compose --version
```


:::note
If you don't have them installed, kindly follow the [installation guide](#quickstart)
:::


Once you've confirmed that they are installed, choose a directory for this project. We recommend

```bash
$ mkdir morra && cd morra
```

Next, download Reach by running

```bash
$ curl https://docs.reach.sh/reach -o reach ; chmod +x reach
```

You'll know that the download worked if you can run

```bash
$ ./reach version
```

Now that we have reach installed, let's start coding!

## {#morra-1} Scaffolding and Building
As said earlier, we'll be buinding a game of Morra with staking. To make Dapp development easier, Reach has a command to bootscrap a simple hello world Dapp. Let's run this command in our project directory

```bash
./reach init
```

:::note
This creates two files for us: `index.rsh` which contains our app backend and `index.mjs` which contains the frontend
:::

Now lets explain what we have in these two files

`index.rsh`
```javascript
'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    // Specify Alice's interact interface here
  });
  const B = Participant('Bob', {
    // Specify Bob's interact interface here
  });
  init();
  // The first one to publish deploys the contract
  A.publish();
  commit();
  // The second one to publish always attaches
  B.publish();
  commit();
  // write your program here
  exit();
});
```

+ Line 1 indicates that this is a Reach program.
You'll always have this at the top of every reach program.
+ Line 3 defines the main export from the program.
When you compile, this is what the compiler will look at.
+ Lines 4 through 9 specify the two participants to this application, _Alice_ and _Bob_.
+ Line 10 marks the deployment of the the Reach program, which allows the program to start doing things.
+ Line 12 - 16 is where Alice and Bob publish and commits the state of the network, more on this later.

`index.mjs`

```javascript
import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);

const [ accAlice, accBob ] =
  await stdlib.newTestAccounts(2, startingBalance);
console.log('Hello, Alice and Bob!');

console.log('Launching...');
const ctcAlice = accAlice.contract(backend);
const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

console.log('Starting backends...');
await Promise.all([
  backend.Alice(ctcAlice, {
    ...stdlib.hasRandom,
    // implement Alice's interact object here
  }),
  backend.Bob(ctcBob, {
    ...stdlib.hasRandom,
    // implement Bob's interact object here
  }),
]);

console.log('Goodbye, Alice and Bob!');
```

+ Line 1 imports the Reach standard library loader.
+ Line 2 imports your backend, which `./reach compile` will produce.
+ Line 3 loads the standard library dynamically.
+ Line 5 defines a quantity of network tokens as the starting balance for each test account.
+ Lines 6 and 7 create test accounts with `startingBalance` for Alice and Bob
+ Line 11 has Alice deploy the application.
+ Line 12 has Bob attach to it.
+ Lines 15 through 18 initialize a backend for Alice.
+ Lines 19 through 22 initialize a backend for Bob.
+ Line 14 waits for the backends to complete.
+ Line 13 and 25 prints a message to the console to indicate the start and end of the backend

This is now enough for Reach to compile and run our program. Let's try by running

```bash
$ ./reach run
```

Reach should now build and launch a Docker container for this application.
You'll see some diagnosis message and some messages from the frontend we wrote in the `index.mjs`

We'll now add some logic to our program

Replace the content of `index.rsh` with the following
```javascript
'reach 0.1';

const Player = {
  ...hasRandom,
  makeGuess: Fun([], UInt),
  showHand: Fun([], UInt),
  getResult: Fun([UInt], Null),
};

export const main = Reach.App(() => {
  const Alice = Participant('Alice', {
    ...Player,
    // Specify Alice's interact interface here
  });
  const Bob = Participant('Bob', {
    ...Player,
    // Specify Bob's interact interface here
  });
  const Charlie = Participant('Charlie', {
    ...Player,
  });
  init();

  // Make Guess
  // The first one to publish deploys the contract
  Alice.only(() => {
    const aliceGuess = declassify(interact.makeGuess());
  })
  Alice.publish(aliceGuess);
  commit();

  // Others always attach
  Bob.only(() => {
    const bobGuess = declassify(interact.makeGuess());
  })
  Bob.publish(bobGuess);
  commit();
  
  Charlie.only(() => {
    const charlieGuess = declassify(interact.makeGuess());
  })
  Charlie.publish(charlieGuess);
  commit();

  // Show hand
  Alice.only(() => {
    const aliceHand = declassify(interact.showHand());
  });
  Alice.publish(aliceHand);
  commit();
  Bob.only(() => {
    const bobHand = declassify(interact.showHand());
  });
  Bob.publish(bobHand);
  commit();
  Charlie.only(() => {
    const charlieHand = declassify(interact.showHand());
  });
  Charlie.publish(charlieHand);
  
  // Calculate outcome
  const total = (aliceHand + bobHand + charlieHand);
  const outcome = 
    total == aliceGuess ? 1 : 
    total == bobGuess ? 2 : 
    total == charlieGuess ? 3 :
    0

  
  commit();

  each([Alice, Bob, Charlie], () => {
    interact.getResult(outcome);
  });
  exit();
});
```
+ Line 3 - 5: we declared the `Player` interface which has 3 functions: `makeGuess`, `showHand` and `getResult`

The interface is what allows the frontend communicate with the backend. We'll see how this functions are implemented late on the frontend
+ Line 19: we created a new Participant, Charlie
+ line 12 and 16 and 20: we added the Player interface to the 3 participants interact interface
+ Line 37, 44 and 50: Because we've added the Player interface to the Participant, each players can now use the use the `interact.makeGuess()` to make their guess
+ Line 57, 62 and 67: players show their hand(while extending a random number of fingers)
+ Line 71 - 76: We get the total number of fingers extended and save the winner to a variable `outcome`

When `outcome` is zero, it means there's no winner, while 1, 2 and 3 means Alice wins, Bob wins and Charlie wins respectively.
+

Replace your `index.mjs` file with this
```javascript
import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);

const [ accAlice, accBob, accCharlie ] =
  await stdlib.newTestAccounts(3, startingBalance);

console.log('Welcome to Morra!');
const ctcAlice = accAlice.contract(backend);
const ctcBob = accBob.contract(backend, ctcAlice.getInfo());
const ctcCharlie = accCharlie.contract(backend, ctcAlice.getInfo());

const OUTCOME =["No winner", "Alice wins", "Bob wins", "Charlie wins"];
const Player = (name) => ({
  makeGuess: () => {
    const guess = Math.floor(Math.random() * 16);
    console.log(`${name} guessed ${guess}`);
    return guess;
  },
  showHand: () => {
    const hand = Math.floor(Math.random() * 6);
    console.log(`${name} showed ${hand} fingers`);
    return hand;
  },
  getResult: (outcome) => {
    console.log(`${name} saw result: ${OUTCOME[outcome]}`);
  }
})
console.log('Starting backends...');
await Promise.all([
  backend.Alice(ctcAlice, {
    ...stdlib.hasRandom,
    ...Player('Alice'),
  }),
  backend.Bob(ctcBob, {
    ...stdlib.hasRandom,
    ...Player('Bob'),
  }),
  backend.Charlie(ctcCharlie, {
    ...stdlib.hasRandom,
    ...Player('Charlie'),
  }),
]);

console.log('Goodbye, Alice, Bob and Charlie!');
```

+ Line 15: We have an array variable `OUTCOME`, which we'd use to interpret the outcome gotten from the backend
+ Line 16 - 30: We define the Player function where we implemented contains `makeGuess`, `showHand` and `getResult`
+ Line 35, 39 and 43: We added these functions to each players backend

Now, lets run our program:
```bash
./reach run
```

The output should look something like this
```bash
Welcome to Morra!
Starting backends...
Alice guessed 9
Bob guessed 9
Charlie guessed 9
Alice showed 4 fingers
Bob showed 0 fingers
Charlie showed 2 fingers
Charlie saw result: No winner
Bob saw result: No winner
Alice saw result: No winner
Goodbye, Alice, Bob and Charlie!
```

## {#morra-2} Add wager

In this section, we will be adding wager to our program, so that the winner of the game can also earn some money. It goes like this: 
The first participant set the wager, then other participants needs to accept before joining the game.

Here are the steps to achieve this
- Allow Alice set a wager
- Create a function for Bob and Charlie to accept wager
- And lastly, transfer tokens to the winner after game end

To allow Alice set a wager which can be accessed by other participant, we add a new variable, `wager` to Alice Participant interface, Alice interface should now look like this

```javascript
  const Alice = Participant('Alice', {
    ...Player,
    wager: UInt   //new
    // Specify Alice's interact interface here
  });
```

To allow Bob and Charlie accept wagger, we'll add an acceptWager function to their interface
```javascript
  const Bob = Participant('Bob', {
    ...Player,
    acceptWager: Fun([UInt], Null)   //new
    // Specify Bob's interact interface here
  });
  const Charlie = Participant('Charlie', {
    ...Player,
    acceptWager: Fun([UInt], Null)   //new
  });
```

Now, when Alice makes his first guess, She should also set the wager and send the amount of token to the smart contract

```javascript
  // Make Guess
  // The first one to publish deploys the contract
  Alice.only(() => {
    const wager = declassify(interact.wager); //new
    const aliceGuess = declassify(interact.makeGuess());

  })
  Alice.publish(wager, aliceGuess)
    .pay(wager); //new
  commit();
```

Bob and Charlie must also accept wager before they can make a guess
```javascript
  // Others always attach
  Bob.only(() => {
    interact.acceptWager(wager); //new
    const bobGuess = declassify(interact.makeGuess());
  })
  Bob.publish(bobGuess)
    .pay(wager); //new
  commit();
  
  Charlie.only(() => {
    interact.acceptWager(wager); //new
    const charlieGuess = declassify(interact.makeGuess());
  })
  Charlie.publish(charlieGuess)
    .pay(wager); //new
  commit();
```

And in the end we transfer all tokens to the winner, or return their tokens(if there's no winner)
Add this code after the code where we compute the outcome, should be around line 79
```javascript
  if (outcome != 0) {
    outcome == 1 ? transfer(wager * 3).to(Alice) :
    outcome == 2 ? transfer(wager * 3).to(Bob) :
    transfer(wager * 3).to(Charlie)
  } else {
    transfer(wager).to(Alice);
    transfer(wager).to(Bob);
    transfer(wager).to(Charlie);
  }

```

Let's also modify our frontend to support our new smart contract
```javascript
import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);

const [ accAlice, accBob, accCharlie ] =
  await stdlib.newTestAccounts(3, startingBalance);

const fmt = (x) => stdlib.formatCurrency(x, 4);
const getBalance = async (name) => fmt(await stdlib.balanceOf(name));

const displayBalance = async (account, name) => {
  const balance = await getBalance(account);
  console.log(`${name} current balance: ${balance}`);
}

await displayBalance(accAlice, "Alice");
await displayBalance(accBob, "Bob");
await displayBalance(accCharlie, "Charlie");

console.log('Welcome to Morra!');
const ctcAlice = accAlice.contract(backend);
const ctcBob = accBob.contract(backend, ctcAlice.getInfo());
const ctcCharlie = accCharlie.contract(backend, ctcAlice.getInfo());

const OUTCOME =["No winner", "Alice wins", "Bob wins", "Charlie wins"];
const Player = (name) => ({
  makeGuess: () => {
    const guess = Math.floor(Math.random() * 16);
    console.log(`${name} guessed ${guess}`);
    return guess;
  },
  showHand: () => {
    const hand = Math.floor(Math.random() * 6);
    console.log(`${name} showed ${hand} fingers`);
    return hand;
  },
  getResult: (outcome) => {
    console.log(`${name} saw result: ${OUTCOME[outcome]}`);
  }
})
console.log('Starting backends...');
await Promise.all([
  backend.Alice(ctcAlice, {
    ...stdlib.hasRandom,
    ...Player('Alice'),
    wager: stdlib.parseCurrency(10)
  }),
  backend.Bob(ctcBob, {
    ...stdlib.hasRandom,
    ...Player('Bob'),
    acceptWager: (amt) => console.log(`Bob accepts the wager of ${fmt(amt)}.`)
  }),
  backend.Charlie(ctcCharlie, {
    ...stdlib.hasRandom,
    ...Player('Charlie'),
    acceptWager: (amt) => console.log(`Bob accepts the wager of ${fmt(amt)}.`)
  }),
]);

await displayBalance(accAlice, "Alice");
await displayBalance(accBob, "Bob");
await displayBalance(accCharlie, "Charlie");

console.log('Goodbye, Alice, Bob and Charlie!');
```
+ Lines 10 through 16 define, we defined 3 functions, `fmt` which formats our currency to 4 decimal places, `getBalance` which get's the balance of a user and fomart it using the `fmt` function, and lastly w have the `displayBalance` function, this function takes in a user, uses the getBalance function to get his balance then displays it on the console
+ Line 48, We let Alice set the wager to 10
+ Line 53 and 58, Bob and Charlie accepts the wager

If we run how program now with
```bash
./reach run
```

The output looks like this
```bash
Alice current balance: 100
Bob current balance: 100
Charlie current balance: 100
Welcome to Morra!
Starting backends...
Alice guessed 15
Bob accepts the wager of 10.
Bob guessed 1
Bob accepts the wager of 10.
Charlie guessed 2
Alice showed 0 fingers
Bob showed 2 fingers
Charlie showed 0 fingers
Charlie saw result: Charlie wins
Alice saw result: Charlie wins
Bob saw result: Charlie wins
Alice current balance: 89.995
Bob current balance: 89.997
Charlie current balance: 119.995
Goodbye, Alice, Bob and Charlie!
```

As we can see, because Charlie won, so his balance increased from 100 to 120

:::note
The participants balances are slightly less than the expected balance because of the transaction fee
:::


## {#morra-3} Secure Morra

Although the current version of our program seems to be working without problem, there is a serious security issue we must tackle. 
What if one of the players(for example Charlie) decided to cheat?. He can easily do this because when Alice and Bob played their hand, they published it to the consensus protocol where Charlie can also see it. Therefore, he can easily manipulate the result to make himself the winner or make sure no one wins.

:::note
In the original Morra game, this problem does not exist because the players show their and simultaneuosly which cannot be easily achieved on a consensus protocol
:::

Therefore we'll be introducing two new functions:
+ `makeCommitment`: This function allows a participant to make a commitment i.e instead of Alice and Bob publishing their hand for everyone to see, They can make a commitment with their hand and only reveal it after Charlie has shown his own hand.
+ `checkCommitment`: This function allows us to check if the value revealed by the user is actually the same as the committed value
:::note
`makeCommitment` uses a one way encryption, so When a participant `makeCommitment` with a value, it's not possible to change the value later
:::

Now let's see how to use it in our program
First we update how Alice and Bob show their hand, Line 29 - 35
```javascript
  Alice.only(() => {
    const wager = declassify(interact.wager);
    const aliceGuess = declassify(interact.makeGuess());
    const _aliceHand = interact.throwHand();
    const [_aliceHandCommit, _aliceSalt] = makeCommitment(interact, _aliceHand);
    const aliceHandCommit = declassify(_aliceHandCommit);

  })
  Alice.publish(wager, aliceGuess, aliceHandCommit)
    .pay(wager);
  commit();

  // Others always attach
  Bob.only(() => {
    interact.acceptWager(wager);
    const bobGuess = declassify(interact.makeGuess());
    const _bobHand = interact.throwHand();
    const [_bobHandCommit, _bobSalt] = makeCommitment(interact, _bobHand);
    const bobHandCommit = declassify(_bobHandCommit);
  })
  Bob.publish(bobGuess, bobHandCommit)
    .pay(wager);
  commit();
```

Charlie can reveal his hand because no one is playing after him. So after Charlie plays his one hand, Alice and Bob would also reveal their hand. But before that, we want to be sure that Alice and Bob hands are not really known to Charlie before they reveal it. And Bob also does not know Alice hand
To do this, we'll be using the `unknowable()` provided by reach
Let's add this after Charlie publishes his hand
```javascript
  unknowable(Charlie, Alice(_aliceHand));
  unknowable(Charlie, Bob(_bobHand));
  unknowable(Bob, Alice(_aliceHand));
```
Now let's reveal Alice ane Bob hands

```javascript
  // Show hand
  Alice.only(() => {
    const aliceSalt = declassify(_aliceSalt);
    const aliceHand = declassify(_aliceHand);
  });
  Alice.publish(aliceHand, aliceSalt);
  checkCommitment(aliceHandCommit, aliceSalt, aliceHand);
  commit();

  Bob.only(() => {
    const bobSalt = declassify(_bobSalt);
    const bobHand = declassify(_bobHand);
  });
  Bob.publish(bobHand, bobSalt);
  checkCommitment(bobHandCommit, bobSalt, bobHand);
```
+ Line 6 and 14 is where Alice and Bob publishes their `hand` and `salt`, which would be used to check if they have revealed the correct hand
+ Line 7 and 15, we use the checkCommitment function to confirm if the hand revealed is the same as the hand they commited earlier

The frontend still remain the same, so we can now run our program using
```bash
./reach run
```

We'll ge the same output we got in {##morra-2}, we can still see all the participant hands printed on the screen but that's because the 3 participants are playing on the same system, in the real sense, only the participant can see their own hand.

## {#tut-7} Continue playing

In this section, we extend our application so that the participant continue to play against each other until there is a clear winner, so if it is a draw they will continue playing. But how do we do this in reach ? Reach supports while loop!

We want to the user continue playing `while` there is no winner(`outcome = 0`), that is our loop would look something like this
```javascript
var outcome = 0;
while(outcome = 0) {
  //Continue playing ...
}

```
However "Reach requires that while loops are annotated with [loop invariants](https://en.wikipedia.org/wiki/Loop_invariant). A loop invariant is a property INV which is true before the loop starts and is true after the loop ends"
In this case we'll be using the contract balance as an invariant, therefore our loop will now be

```javascript
var outcome = 0;
invariant( balance() == 3 * wager && outcome >= 0 && outcome < 4 );
while(outcome = 0) {
  //Continue playing ...
}
```

The full code for `index.rsh` will now look like this
```javascript
'reach 0.1';

const Player = {
  ...hasRandom,
  makeGuess: Fun([], UInt),
  throwHand: Fun([], UInt),
  getResult: Fun([UInt], Null),
  informTimeout: Fun([], Null),
};

export const main = Reach.App(() => {
  const Alice = Participant('Alice', {
    ...Player,
    wager: UInt
    // Specify Alice's interact interface here
  });
  const Bob = Participant('Bob', {
    ...Player,
    acceptWager: Fun([UInt], Null)
    // Specify Bob's interact interface here
  });
  const Charlie = Participant('Charlie', {
    ...Player,
    acceptWager: Fun([UInt], Null)
  });
  init();

  // Make Guess
  // The first one to publish deploys the contract
  Alice.only(() => {
    const wager = declassify(interact.wager);
    const aliceGuess = declassify(interact.makeGuess());
  })
  Alice.publish(wager, aliceGuess)
    .pay(wager);
  commit();

  // Others always attach
  Bob.only(() => {
    interact.acceptWager(wager);
    const bobGuess = declassify(interact.makeGuess());
  })
  Bob.publish(bobGuess)
    .pay(wager);
  commit();
  
  Charlie.only(() => {
    interact.acceptWager(wager);
    const charlieGuess = declassify(interact.makeGuess());
  })
  Charlie.publish(charlieGuess)
    .pay(wager);

  // Start throwing hand until there's a winner
  var outcome = 0;
  invariant( balance() == 3 * wager && outcome >= 0 && outcome < 4 );
  while(outcome == 0) {
    commit();

    Alice.only(() => {
      const _aliceHand = interact.throwHand();
      const [_aliceHandCommit, _aliceSalt] = makeCommitment(interact, _aliceHand);
      const aliceHandCommit = declassify(_aliceHandCommit);
    })
    Alice.publish(aliceHandCommit)
    commit();

    unknowable(Bob, Alice(_aliceHand, _aliceSalt));
    unknowable(Charlie, Alice(_aliceHand, _aliceSalt));

    Bob.only(() => {
      const _bobHand = interact.throwHand();
      const [_bobHandCommit, _bobSalt] = makeCommitment(interact, _bobHand);
      const bobHandCommit = declassify(_bobHandCommit);
    })
    Bob.publish(bobHandCommit)
    commit();

    unknowable(Alice, Bob(_bobHand, _bobSalt));
    unknowable(Charlie, Bob(_bobHand, _bobSalt));

    Charlie.only(() => {
      const charlieHand = declassify(interact.throwHand());
    })
    Charlie.publish(charlieHand)
    commit();
    
    // Show hands
    Alice.only(() => {
      const aliceSalt = declassify(_aliceSalt);
      const aliceHand = declassify(_aliceHand);
    });
    Alice.publish(aliceHand, aliceSalt);
    checkCommitment(aliceHandCommit, aliceSalt, aliceHand);
    commit();

    Bob.only(() => {
      const bobSalt = declassify(_bobSalt);
      const bobHand = declassify(_bobHand);
    });
    Bob.publish(bobHand, bobSalt);
    checkCommitment(bobHandCommit, bobSalt, bobHand);
  
    // Calculate outcome
    const total = (aliceHand + bobHand + charlieHand);
    outcome = 
      total == aliceGuess ? 1 : 
      total == bobGuess ? 2 : 
      total == charlieGuess ? 3 :
      0
    continue;
  }

  outcome == 1 ? transfer(wager * 3).to(Alice) :
    outcome == 2 ? transfer(wager * 3).to(Bob) :
    transfer(wager * 3).to(Charlie)
  commit();

  each([Alice, Bob, Charlie], () => {
    interact.getResult(outcome);
  });
  exit();
});

```
We can now run our program using
```bash
./reach run
```

Output

```bash
Alice current balance: 100
Bob current balance: 100
Charlie current balance: 100
Welcome to Morra!
Starting backends...
Alice guessed 0
Bob accepts the wager of 10.
Bob guessed 5
Bob accepts the wager of 10.
Charlie guessed 11
Alice throwed 2 fingers
Bob throwed 2 fingers
Charlie throwed 4 fingers
Alice throwed 0 fingers
Bob throwed 5 fingers
Charlie throwed 4 fingers
Alice throwed 1 fingers
Bob throwed 4 fingers
Charlie throwed 0 fingers
Bob saw result: Bob wins
Alice saw result: Bob wins
Charlie saw result: Bob wins
Alice current balance: 89.9602
Bob current balance: 119.9601
Charlie current balance: 89.9801
Goodbye, Alice, Bob and Charlie!
```
From the output, we can see that the participant continues playing until there is a winner

We've been using a console frontend so far, now it's time a build a nice frontend for our appication