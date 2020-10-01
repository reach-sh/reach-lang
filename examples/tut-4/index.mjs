import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as backend from './build/index.main.mjs';

(async () => {
  const startingBalance = stdlib.parseCurrency(10);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const getBalance = async (who) => fmt(await stdlib.balanceOf(who));
  const beforeAlice = await getBalance(accAlice);
  const beforeBob = await getBalance(accBob);

  const ctcAlice = await accAlice.deploy(backend);
  const ctcBob = await accBob.attach(backend, ctcAlice.getInfo());

  const HAND = ['Rock', 'Paper', 'Scissors'];
  const OUTCOME = ['Bob wins', 'Draw', 'Alice wins'];
  const Player = (Who) => ({
    ...stdlib.hasRandom, // <--- new!
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
    backend.Alice(stdlib, ctcAlice, {
      ...Player('Alice'),
      wager: stdlib.parseCurrency(5),
    }),
    backend.Bob(stdlib, ctcBob, {
      ...Player('Bob'),
      acceptWager: (amt) => {
        console.log(`Bob accepts the wager of ${fmt(amt)}.`);
      },
    }),
  ]);

  const afterAlice = await getBalance(accAlice);
  const afterBob = await getBalance(accBob);

  console.log(`Alice went from ${beforeAlice} to ${afterAlice}.`);
  console.log(`Bob went from ${beforeBob} to ${afterBob}.`);

})();
