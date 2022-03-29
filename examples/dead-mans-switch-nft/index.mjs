import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const [accAlice, accBob, accCharlie] =
  await stdlib.newTestAccounts(3, stdlib.parseCurrency(100));
const ctcAlice   = accAlice.contract(backend);
const ctcBob     = accBob.contract(backend, ctcAlice.getInfo());
const ctcCharlie = accCharlie.contract(backend, ctcAlice.getInfo());

// Launch the ctc
try {
  await backend.Creator(ctcAlice, {
    firstHeir: accBob,
    ready: () => { throw 'ready' }
  });
} catch (e) {
  if (e != 'ready') {
    throw e;
  }
}

const runOwner = async (ctc, name, nextHeir) => {
  console.log(`${name} is now the owner`);

  if (nextHeir) {
    await ctc.apis.Owner.setNextHeir(nextHeir);
  }

  for (let n = 0; n < 5; n++) {
    console.log(`${name} ping #${n}`);
    await ctc.apis.Owner.ping();
  }

  console.log(`${name} goes silent`);
  await stdlib.wait(5);
};

await runOwner(ctcAlice, 'Alice');
await runOwner(ctcBob, 'Bob', accCharlie);
await runOwner(ctcCharlie, 'Charlie', accAlice);
