import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);
const assert = stdlib.assert;

const algoP = stdlib.connector === "ALGO";

const [ accA, accB ] = await stdlib.newTestAccounts(2, stdlib.parseCurrency(100));
const ctc = accA.contract(backend);

try {
  await ctc.p.A({ready: () => {throw 42;},});
} catch (e) {
  if ( e !== 42) {throw e;}
}

// accA is opted in after launching.
assert(await stdlib.appOptedIn(accA, await ctc.getInfo()));
// appOptedIn should also work with an address.
assert(await stdlib.appOptedIn(accA.getAddress(), await ctc.getInfo()));
assert(await accA.appOptedIn(await ctc.getInfo()));

// accB is not opted in.
if (algoP) {
  assert(! (await stdlib.appOptedIn(accB, await ctc.getInfo())));
  assert(! (await stdlib.appOptedIn(accB.getAddress(), await ctc.getInfo())));
  assert(! (await accB.appOptedIn(await ctc.getInfo())));
}

const ctcB = accB.contract(backend, await ctc.getInfo());
await ctcB.appOptIn();
// A second opt-in should be a no-op, not a crash.
await ctcB.appOptIn();

assert(await stdlib.appOptedIn(accB, await ctc.getInfo()));
assert(await stdlib.appOptedIn(accB.getAddress(), await ctc.getInfo()));
assert(await accB.appOptedIn(await ctc.getInfo()));

