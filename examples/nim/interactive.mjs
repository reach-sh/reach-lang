import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as NIM from './build/index.main.mjs';
import { ask, yesno, runPart
       } from '@reach-sh/stdlib/command_line_helpers.mjs';

// Set up dependencies to run locally:
// (cd ../../js && npm link)
// npm install
// npm link "@reach-sh/stdlib"

const {fromWei, toWeiBN, lt, gt, toBN, hasRandom} = stdlib;

const name = process.argv[2];

const getParams = async () => {
  const wagerAmount = await ask(
    `What is the wager amount in ETH? (default: 5) > `,
    (x) => toWeiBN(x || '5', 'ether')
  );
  const initialHeapSize = await ask(
    `What is the initial heap size for this game? (default: 21) > `,
    (x) => toBN(x || 21)
  );
  console.log(`${name} publishes parameters of game:
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
      if (heaps[choice - 1] <= 0) {
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

runPart({
  PROG: NIM,
  allPartNames: ['A', 'B'],
  deployerPartName: 'A',
  thisPartName: name,
  greetBegin: `Let's play a game of nim.`,
  interact,
  stdlib,
});
