import { loadStdlib } from '@reach-sh/stdlib';
import { util } from '@reach-sh/stdlib';
const { thread, Signal, Timeout } = util;
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib(process.env);

const ready = new Signal();

const startingBalance = stdlib.parseCurrency(100);

const [ accAlice, accBob ] =
  await stdlib.newTestAccounts(2, startingBalance);

const ctcAdmin = accAlice.contract(backend);
const ctcBob = accBob.contract(backend, ctcAdmin.getInfo());

const expectedError = `Expected the DApp to be in state(s) [1,3], but it was actually in state 2.

State 1 corresponds to the commit() at : Module
  at ./index.rsh:15:11:after expr stmt semicolon,
State 3 corresponds to the commit() at : Module
  at ./index.rsh:23:11:after expr stmt semicolon
State 2 corresponds to the commit() at : Module
  at ./index.rsh:19:11:after expr stmt semicolon`

await Promise.all([
  backend.Alice(ctcAdmin, {
    done: () => ready.notify()
  }),
  thread(async () => {
    await ready.wait();
    const f = ctcBob.a.B.f;
    await f();
    try {
      await f();
    } catch (e) {
      const msg = e.toString();
      console.log(`Error thrown: `, msg);
      stdlib.assert(msg.includes(expectedError), "Contains expected error message");
      process.exit();
    }
  }),
]);
