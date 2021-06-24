import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

export const run = async (n, stdlib) => {
  const startingBalance = stdlib.parseCurrency(100);

  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const getBalance = async (who) => fmt(await stdlib.balanceOf(who));

  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const beforeAlice = await getBalance(accAlice);
  const beforeBob = await getBalance(accBob);

  const ctcAlice = accAlice.deploy(backend);
  const ctcBob = accBob.attach(backend, ctcAlice.getInfo());

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

(async () => {
  const stdlib = await loadStdlib();
  await run(0, stdlib);
  await run(1, stdlib);
  await run(2, stdlib);
})();
