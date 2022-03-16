import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();

const [ accA, accB ] =
  await stdlib.newTestAccounts(2, stdlib.parseCurrency(100));
accA.setDebugLabel('Alice');
accB.setDebugLabel('Bob');

const runOnce = async (dir) => {
  const ctcA = accA.contract(backend);
  const ctcB = accB.contract(backend, ctcA.getInfo());

  await Promise.all([
    ctcA.participants.A({
      x: 5,
      fork: async (xs) => {
        if ( dir ) {
          await ctcB.a.f(xs);
        } else {
          await ctcB.a.g();
        }
      },
    }),
    ctcB.p.B({
      ...stdlib.hasConsoleLogger,
    }),
  ]);
};
await runOnce(true);
await runOnce(false);
