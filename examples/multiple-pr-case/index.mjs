import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

export const run = async (n) => {
  const stdlib = await loadStdlib();
  if ( stdlib.connector === 'ALGO' ) {
    console.log(`XXX Unsupported`);
    process.exit(0);
  }
  const startingBalance = stdlib.parseCurrency(100);
  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const getBalance = async (who) => fmt(await stdlib.balanceOf(who));

  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = accAlice.deploy(backend);
  const ctcBob = accBob.attach(backend, ctcAlice.getInfo());

  const beforeA = await getBalance(accAlice);
  const beforeB = await getBalance(accBob);

  const doCase = {
    1: true,
    2: true,
    3: true,
    4: true,
  };

  const common = (Who) => ({
    doCase : (i) => {
      const res = doCase[i];
      const willCompleteText = `${Who} will complete`;
      const completedText = `${Who} already completed`;
      console.log(`Case ${i}: ${res ? willCompleteText : completedText}`);
      doCase[i] = res ? !res : false;
      return res;
    }
  });

  await Promise.all([
    backend.A(ctcAlice, {
      timeout: 10,
      payment: stdlib.parseCurrency(1),
      ...common('Alice'),
    }),
    backend.B(ctcBob, {
      ...common('Bob')
    }),
  ]);

  const afterA = await getBalance(accAlice);
  const afterB = await getBalance(accBob);

  console.log(`Alice went from ${beforeA} to ${afterA}`);
  console.log(`Bob went from ${beforeB} to ${afterB}`);
};

(async () => {
  await run();
})();
