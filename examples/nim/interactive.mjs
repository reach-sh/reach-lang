import * as stdlib from '../../js/ETH.mjs';
// import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as NIM from './build/index.main.mjs';
import {ask, yesno, noFurtherQuestions} from './prompt.mjs';

const {fromWei, toWeiBN, newTestAccount, lt, gt, toBN, hasRandom} = stdlib;

const name = process.argv[2];

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

const getParams = async () => {
  const wagerAmount = await ask(
    `What is the wager amount in ETH? (default: 5) > `,
    (x) => toWeiBN(x || '5', 'ether')
  );
  const initialHeapSize = await ask(
    `What is the initial heap size for this game? (default: 21) > `,
    (x) => toBN(x || 21)
  );
  console.log(`{name} publishes parameters of game:
wager of ${fromWei(wagerAmount)}ETH and heap is ${initialHeapSize}`);

  return [ wagerAmount, initialHeapSize ];
};

const acceptParams = async (givenWagerAmount, givenInitialHeapSize) => {
  const response = await ask(
    `Do you accept the terms?
  wagerAmount: ${fromWei(givenWagerAmount)}ETH
  initialHeapSize: ${givenInitialHeapSize}
  (default y) > `,
    (x) => yesno(x || 'y')
  );
  if (!response) {
    throw Error(`Terms not accepted, ${name} is outta here`);
  }

  console.log(`${name} accepts parameters of game:
wager of ${fromWei(givenWagerAmount)}ETH and heaps of ${givenInitialHeapSize}`);
};

const getMove = async (heap1, heap2) => {
  console.log(`heap1: ${heap1}, heap2: ${heap2}`);

  const heaps = [heap1, heap2];
  const move = await ask(
    `Which heap? (default: 1) > `,
    (x) => {
      let choice = 1;
      if (!x) {choice = 1;}
      else if (x == 1) {choice = 1;}
      else if (x == 2) {choice = 2;}
      else {throw Error('You must enter 1 or 2');}
      if (heaps[choice] <= 0) {
        throw Error('You must choose the other one, that one is empty.');
      } else {
        return choice;
      }
    }
  );
  const heap = heaps[move - 1];
  const amt = await ask(
    `What amount? (default 1) > `,
    (xStr) => {
      const x = toBN(xStr || 1);
      if (gt(x, heap)) { throw Error(`Not allowed: ${x} > ${heap}`); }
      else if (lt(x, 1)) { throw Error(`Not allowed: ${x} < 1`); }
      else { return x; }
    }
  );

  console.log(`${name} chooses heap ${move} with amount ${amt}`);
  return [ move == 1, amt ];
};

const showOutcome = (outcome) => {
  console.log(`${name} sees the final outcome: ${outcome}`);
};

const interact = {
  ...hasRandom,
  getParams,
  acceptParams,
  getMove,
  showOutcome
};

const displayCtc = (ctc) => {
  console.log(`{"address": "${ctc.address}", "creation_block": ${ctc.creation_block}}`);
};

const runAlice = async () => {
  console.log(`Hello, Alice. Let's start a game of nim.`);
  console.log(`First, we'll connect to the test net.`);

  const account = await promptCreateTestAccount();

  console.log(`Next, we'll deploy the contract.`);
  const ctc = await account.deploy(NIM);

  console.log(`Show Bob the deployed contract info:`);
  displayCtc(ctc);

  console.log(`Alright, let's play!`);
  await NIM.A(stdlib, ctc, interact);
  noFurtherQuestions();
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

const runBob = async () => {
  console.log(`Hello, Bob. Let's start a game of nim.`);
  console.log(`First, we'll connect to the test net.`);

  const account = await promptCreateTestAccount();

  console.log(`Now, ask Alice about the contract info.`);
  console.log(`Next, we'll attach to Alice's contract.`);
  const ctcAlice = await askCtc();
  const ctc = await account.attach(NIM, ctcAlice);

  console.log(`Alright, let's play!`);
  await NIM.B(stdlib, ctc, interact);
  noFurtherQuestions();
};

if (name === 'Alice') {
  runAlice();
} else if (name === 'Bob') {
  runBob();
} else {
  throw Error(`Expected Alice or Bob, got ${name}`);
}
