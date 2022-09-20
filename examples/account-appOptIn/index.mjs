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
assert(await stdlib.accountAppOptedIn(accA, ctc));
assert(await accA.appOptedIn(ctc));

// accB is not opted in.
if (algoP) {
  assert(! (await stdlib.accountAppOptedIn(accB, ctc)));
  assert(! (await accB.appOptedIn(ctc)));
}

await accB.appOptIn(ctc);

assert(await stdlib.accountAppOptedIn(accB, ctc));
assert(await accB.appOptedIn(ctc));
