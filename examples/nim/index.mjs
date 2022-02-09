import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();

const startingBalance = stdlib.parseCurrency(100);
const wagerAmount = stdlib.parseCurrency(5);

const dispAmt = (x) => `${stdlib.formatCurrency(x)} ${stdlib.standardUnit}`;
console.log(`\nMaking accounts\n`);

const alice = await stdlib.newTestAccount(startingBalance);
const bob = await stdlib.newTestAccount(startingBalance);

console.log(`\nDeploying and attaching\n`);

const ctcAlice = alice.contract(backend);
const ctcBob = bob.contract(backend, ctcAlice.getInfo());

console.log(`\nRunning a random game\n`);

const interactWith = (name) => ({
  ...stdlib.hasRandom,
  getParams: () => {
    console.log(`${name} publishes parameters of game: wager of ${dispAmt(wagerAmount)} and heap is 21`);
    return [ wagerAmount, stdlib.bigNumberify(21) ];
  },
  acceptParams: (givenWagerAmount, givenInitialHeap) => {
    console.log(`${name} accepts parameters of game: wager of ${dispAmt(givenWagerAmount)} and heap of ${givenInitialHeap}`);
  },
  getMove: (heap1, heap2) => {
    console.log(`${name} chooses a heap from: ${heap1} and ${heap2} with amount 1`);
    return [ stdlib.gt(heap1, heap2), stdlib.bigNumberify(1) ];
  },
  showOutcome: (outcome) => {
    console.log(`${name} sees the final outcome: ${outcome}`);
  },
});

await Promise.all([
  backend.A(ctcAlice, interactWith('Alice')),
  backend.B(ctcBob, interactWith('Bob')),
]);
