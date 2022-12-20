import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();
const reqs = [];
stdlib.setSigningMonitor(async (evt, pre, post) =>
  reqs.push({evt, pre: await pre, post: await post}));
const startingBalance = stdlib.parseCurrency(100);

const accA = await stdlib.newTestAccount(startingBalance);
const accB = await stdlib.newTestAccount(startingBalance);
const accC = await stdlib.newTestAccount(startingBalance);
const accD = await stdlib.createAccount();

const zorkmid = await stdlib.launchToken(accA, "zorkmid", "ZMD");
const ctcA = accA.contract(backend);
const ctcB = accB.contract(backend, ctcA.getInfo());
await Promise.all([
  ctcA.p.A({
    mid: () => null,
  }),
  ctcB.p.B({}),
]);
const stepsInProgram = 4;

console.log(reqs);
const actual = reqs.length;
const expected = {
  'ALGO': (stepsInProgram + 1 /*alloc*/ + 3 /*newTestAccount*/ + 1 /*launchToken*/),
  'CFX': stepsInProgram,
  'ETH': stepsInProgram + 3 /*newTestAccount?*/,
}[stdlib.connector];
console.log(actual, expected);
stdlib.assert(actual === expected, 'wrong number of requests');
