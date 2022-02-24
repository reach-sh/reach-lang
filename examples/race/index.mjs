import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

  const stdlib = loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);
  accAlice.setDebugLabel('Alice');
  accBob.setDebugLabel('Bob');

  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const getBalance = async (who) => fmt(await stdlib.balanceOf(who));
  const beforeAlice = await getBalance(accAlice);
  const beforeBob = await getBalance(accBob);

  const ctcAlice = accAlice.contract(backend);
  const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

  const OUTCOME = ['Alice wins', 'Bob wins', 'Timeout'];
  const Player = (Who) => ({
    showOutcome: (outcome) => {
      console.log(`${Who} saw outcome ${OUTCOME[outcome]}`);
    },
  });

  await Promise.all([
    backend.Alice(ctcAlice, {
      ...Player('Alice'),
      getParams: () => ({
        wager: stdlib.parseCurrency(5),
        deadline: 5,
      }),
    }),
    backend.Bob(ctcBob, {
      ...Player('Bob'),
      confirmWager: (amt) => {
        console.log(`Bob accepts the wager of ${fmt(amt)}.`);
      },
    }),
  ]);

  const afterAlice = await getBalance(accAlice);
  const afterBob = await getBalance(accBob);

  console.log(`Alice went from ${beforeAlice} to ${afterAlice}.`);
  console.log(`Bob went from ${beforeBob} to ${afterBob}.`);

