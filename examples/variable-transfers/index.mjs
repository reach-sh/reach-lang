import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();
const startingBalance = stdlib.parseCurrency(100);

const fmt = (x) => stdlib.formatCurrency(x, 4);
const getBalance = async (who) => fmt(await stdlib.balanceOf(who));

const run = async (n) => {
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const beforeAlice = await getBalance(accAlice);
  const beforeBob = await getBalance(accBob);

  const ctcAlice = accAlice.contract(backend);
  const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

  await Promise.all([
    backend.A(ctcAlice, {
      n,
      payment: stdlib.parseCurrency(75)
    }),
    backend.B(ctcBob, {
    }),
  ]);

  const afterAlice = await getBalance(accAlice);
  const afterBob = await getBalance(accBob);

  const info = {
    0: 'Alice wins',
    1: 'Bob wins',
    2: 'Draw'
  }

  console.log(`Run results: ${info[n]}`);
  console.log(`Alice from ${beforeAlice} to ${afterAlice}`);
  console.log(`Bob from ${beforeBob} to ${afterBob}`);
};

await run(0);
await run(1);
await run(2);
