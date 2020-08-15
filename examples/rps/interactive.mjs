// import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as stdlib from '../../js/ETH.mjs';
import * as RPS from './build/index.once.mjs';
import {ask, yesno, noFurtherQuestions} from './prompt.mjs';

// TODO: less copy/paste between examples; use a shared lib.

const {fromWei, toWeiBN, newTestAccount, hasRandom} = stdlib;

const name = process.argv[2];
const toUnit = fromWei;
const unit = 'ETH';

const promptCreateTestAccount = async () => {
  const start_amt = await ask(
    `How much ETH would you like in your test account? (default: 100) > `,
    (x) => toWeiBN(x || '100', 'ether')
  );

  console.log(`Creating account...`);
  const account = await newTestAccount(start_amt);
  console.log(`...account created.`);

  return account;
};

const displayCtc = (ctc) => {
  console.log(`{"address": "${ctc.address}", "creation_block": ${ctc.creation_block}}`);
};

const askCtc = async () => {
  return await ask(
    `Paste Alice's contract info here > `,
    (objStr) => {
      const obj = JSON.parse(objStr);
      if (!obj.address) { throw Error(`Missing address`); }
      else if (!obj.creation_block) { throw Error(`Missing creation_block`); }
      else { return obj; }
    }
  );
};

// interact methods

const getParams = async () => {
  const wagerAmount = await ask(
    `What is the wager amount in ETH? (default: 5) > `,
    (x) => toWeiBN(x || '5', 'ether')
  );
  const escrowAmount = await ask(
    `What is the escrow amount in ETH? (default: 0.15) > `,
    (x) => toWeiBN(x || '0.15', 'ether')
  );

  console.log(`(local: ${name} sets the parameters of the game: 
  wagerAmount: ${toUnit(wagerAmount)} ${unit}
  escrowAmount: ${toUnit(escrowAmount)} ${unit}.)`);
  return [wagerAmount, escrowAmount];
};

const acceptParams = async (wager, escrow) => {
  const response = await ask(
    `Do you accept the terms?
  wagerAmount: ${toUnit(wager)} ${unit}
  escrowAmount: ${toUnit(escrow)} ${unit}
  [y/n] (default: y) > `,
    (x) => yesno(x || 'y')
  );
  if (!response) {
    throw Error(`Terms not accepted, ${name} is outta here`);
  }

  console.log(`${name} accepts parameters of game.`);
};

const partnerIs = async (who) => {
  console.log(`${name} is playing with ${who}`);
};

const getHand = async () => {
  const res = await ask(
    `What's your hand? [ROCK, PAPER, SCISSORS] (default: ROCK) > `,
    (x) => {
      if (!x) { return 'ROCK'; }
      else if (x == 'ROCK') { return 'ROCK'; }
      else if (x == 'PAPER') { return 'PAPER'; }
      else if (x == 'SCISSORS') { return 'SCISSORS'; }
      else { throw Error(`Expected ROCK, PAPER, or SCISSORS, got ${x}`); }
    }
  );
  console.log(`(local: ${name} plays ${res}.)`);
  return res;
};

const commits = async () => {
  console.log(`${name} commits to play with (hidden) hand.`);
};

const shows = async () => {
  console.log(`${name} sends hand in clear.`);
};

const reveals = async (handB) => {
  console.log(`${name} reveals salt and hand, after learning B played ${handB}.`);
};

const endsWith = async (outcome) => {
  console.log(`${name} agrees that game is over and outcome is ${outcome}.`);
};

const interact = {
  ...hasRandom,
  getHand,
  partnerIs,
  endsWith,
  // Alice
  getParams,
  commits,
  reveals,
  // Bob
  acceptParams,
  shows,
};


const runAlice = async () => {
  console.log(`Hello, Alice. Let's start a game of Rock, Paper, Scissors.`);
  console.log(`First, we'll connect to the test net.`);
  const account = await promptCreateTestAccount();

  console.log(`Next, we'll deploy the contract.`);
  const ctc = await account.deploy(RPS);

  console.log(`Show Bob the deployed contract info:`);
  displayCtc(ctc);

  console.log(`Alright, let's play!`);
  await RPS.A(stdlib, ctc, interact);
  noFurtherQuestions();
};


const runBob = async () => {
  console.log(`Hello, Bob. Let's start a game of Rock, Paper, Scissors.`);
  console.log(`First, we'll connect to the test net.`);
  const account = await promptCreateTestAccount();

  console.log(`Now, ask Alice about the contract info.`);
  console.log(`Next, we'll attach to Alice's contract.`);
  const ctcAlice = await askCtc();
  const ctc = await account.attach(RPS, ctcAlice);

  console.log(`Alright, let's play!`);
  await RPS.B(stdlib, ctc, interact);
  noFurtherQuestions();
};

if (name === 'Alice') {
  runAlice();
} else if (name === 'Bob') {
  runBob();
} else {
  throw Error(`Expected Alice or Bob, got ${name}`);
}
