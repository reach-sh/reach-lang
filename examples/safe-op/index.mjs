import { loadStdlib } from '@reach-sh/stdlib';
import * as addBackend from './build/index.safeAddTest.mjs';
import * as subBackend from './build/index.safeSubTest.mjs';
import * as divBackend from './build/index.safeDivTest.mjs';
import * as modBackend from './build/index.safeModTest.mjs';
import * as castBackend from './build/index.safeCastTest.mjs';

const stdlib = loadStdlib();
const startingBalance = stdlib.parseCurrency(100);
const alice = await stdlib.newTestAccount(startingBalance);
const um = stdlib.reachStdlib.UInt_max;
const bn = stdlib.bigNumberify;

const mkTest = async (backend, x, bx, msg) => {
  const ctcAlice = alice.contract(backend);
  console.log("Start!");

  let threwError = false;
  await Promise.all([
    backend.A(ctcAlice, {
      x,
      bx,
      show: console.log,
    }),
  ])
  .catch(e => {
    console.log(e);
    if (e.toString().includes(msg)) {
      threwError = true;
    }
  });

  if (!threwError) {
    throw Error(`Expected to fail: ${msg}`);
  }
}

await mkTest(addBackend, um.sub(bn(4)), bn(0), 'add overflow');
await mkTest(subBackend, bn(4), bn(0), 'sub wraparound');
await mkTest(divBackend, bn(0), bn(0), 'div by zero');
await mkTest(modBackend, bn(0), bn(0), 'div by zero');
await mkTest(castBackend, bn(0), um.add(5), 'out of range');
