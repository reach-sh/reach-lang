import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  if ( stdlib.connector === 'ALGO' ) { process.exit(0); }
  const startingBalance = stdlib.parseCurrency(100);

  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const getBalance = async (who) =>
        stdlib.formatCurrency(await stdlib.balanceOf(who), 4);
  const beforeAlice = await getBalance(accAlice);
  const beforeBob = await getBalance(accBob);

  const ctcAlice = accAlice.contract(backend);
  const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

  const thePass = stdlib.bigNumberify('31916810322672614595785596490503352186605157015187771518249890054489700143553');

  await Promise.all([
    backend.Alice(ctcAlice, {
      amt: stdlib.parseCurrency(25),
      pass: thePass,
    }),
    backend.Bob(ctcBob, {
      ...stdlib.hasConsoleLogger,
      getPass: () => {
        console.log(`Bob asked to give the preimage.`);
        console.log(`Returning: ${thePass}`);
        return thePass;
      },
    }),
  ]);

  const afterAlice = await getBalance(accAlice);
  const afterBob = await getBalance(accBob);

  console.log(`Alice went from ${beforeAlice} to ${afterAlice}.`);
  console.log(`Bob went from ${beforeBob} to ${afterBob}.`);

})();
