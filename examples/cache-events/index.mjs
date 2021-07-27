import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import * as ask from '@reach-sh/stdlib/ask.mjs';

const runA = async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(10);

  const accAlice = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = accAlice.deploy(backend);
  const ctcInfo = await ctcAlice.getInfo();

  console.log(`info: `, ctcInfo);

  let i = 0;
  await Promise.all([ backend.A(ctcAlice, {
    get: () => {
      i += 1;
      return i;
    }
  }) ]);
  console.log(`A Finished`);
}

const runB = async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(10);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const info = await ask.ask(`Enter contract info:`, (stdlib.connector == 'ALGO' ? parseInt : ((x) => x)));
  const ctcBob = accBob.attach(backend, info);
  await Promise.all([ backend.B(ctcBob, {}) ]);
  console.log(`B Finished`);
}

(async () => {
  if(process.argv.length > 2) {
    await runB();
  } else {
    await runA();
  }
  process.exit();
})();
