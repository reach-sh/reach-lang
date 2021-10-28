import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

// lightly modified from tut-3
(async () => {
  const startingBalance = stdlib.parseCurrency(100);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = accAlice.contract(backend);
  const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

  const HAND = ['Rock', 'Paper', 'Scissors'];
  const OUTCOME = ['Bob wins', 'Draw', 'Alice wins'];
  const Player = (Who) => ({
    getHand: () => {
      const hand = Math.floor(Math.random() * 3);
      console.log(`${Who} played ${HAND[hand]}`);
      return hand;
    },
    seeOutcome: (outcome) => {
      console.log(`${Who} saw outcome ${OUTCOME[outcome]}`);
    },
  });

  await Promise.all([
    backend.Alice(ctcAlice, {
      ...Player('Alice'),
    }),
    backend.Bob(ctcBob, {
      ...Player('Bob'),
    }),
  ]);

  console.log(`Again!`);

  const ctcAlice2 = accAlice.contract(backend);
  const ctcBob2 = accBob.contract(backend, ctcAlice2.getInfo());

  await Promise.all([
    backend.Alice(ctcAlice2, {
      ...Player('Alice2'),
    }),
    backend.Bob(ctcBob2, {
      ...Player('Bob2'),
    }),
  ]);

})();
