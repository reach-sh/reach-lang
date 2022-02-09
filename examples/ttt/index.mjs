import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

function render(st) {
  let o = '\n\t';
  for ( let i = 0; i < 9; i++ ) {
    o += st.xs[i] ? 'X' : st.os[i] ? 'O' : ' ';
    if ( i != 8 ) {
      o += (i % 3 == 2) ? '\n\t-----\n\t' : '|';
    }
  }
  o += '\n';
  return o;
}

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
  getWager: () => {
    console.log(`${name} publishes parameters of game: wager of ${dispAmt(wagerAmount)}`);
    return wagerAmount;
  },
  acceptWager: (givenWagerAmount) => {
    console.log(`${name} accepts parameters of game: wager of ${dispAmt(givenWagerAmount)}`);
  },
  getMove: (state) => {
    console.log(`${name} chooses a move from the state:${render(state)}`);
    const xs = state.xs;
    const os = state.os;
    while ( xs && os ) {
      const i = Math.floor( Math.random() * 9 );
      if ( ! ( xs[i] || os[i] ) ) {
        return i;
      }
    }
    throw Error(`impossible to make a move`);
  },
  endsWith: (state) => {
    console.log(`${name} sees the final state: ${render(state)}`);
  },
});

await Promise.all([
  backend.A(ctcAlice, interactWith('Alice')),
  backend.B(ctcBob, interactWith('Bob')),
]);
