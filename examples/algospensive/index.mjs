import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();

const [ accA, accB ] =
  await stdlib.newTestAccounts(2, stdlib.parseCurrency(100));
accA.setDebugLabel('Alice');
accB.setDebugLabel('Bob');

const ctcA = accA.contract(backend);
const ctcB = accB.contract(backend, ctcA.getInfo());

await Promise.all([
  ctcA.participants.A({
    x: 5,
  }),
  ctcB.p.B({
    ...stdlib.hasConsoleLogger,
  }),
]);
