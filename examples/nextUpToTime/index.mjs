import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);
const assert = stdlib.assert;

const assertEq = (a, b, context = 'assertEq') => {
  if (a === b) return;
  try {
    const res1BN = bigNumberify(a);
    const res2BN = bigNumberify(b);
    if (res1BN.eq(res2BN)) return;
  } catch {}
  try {
    const stripNulls = (s) => s.replace(/\0*$/g, "");
    if (stripNulls(`${a}`) === stripNulls(`${b}`)) return;
  } catch {}
  try {
    if (JSON.stringify(a) === JSON.stringify(b)) return;
  } catch {}
  try {
    if (parseInt(a) == parseInt(b)) return;
  } catch {}
  assert(false, `${context}: ${a} == ${b}`);
};


const acc = await stdlib.newTestAccount(stdlib.parseCurrency(100));

const ctc = acc.contract(backend);
await stdlib.withDisconnect(() => ctc.participants.D({
  ready: () => {stdlib.disconnect()}
}));

await ctc.a.poke();
const t1 = await stdlib.getNetworkTime();
await stdlib.wait(1);

assertEq((await ctc.e.e.nextUpToTime(t1)).what, 0);
assertEq((await ctc.e.e.nextUpToTime(t1)).what, 1);
assertEq((await ctc.e.e.nextUpToTime(t1)), undefined);

await ctc.a.poke();
await stdlib.wait(1);
const t2 = await stdlib.getNetworkTime();
await ctc.a.poke();
const t3 = await stdlib.getNetworkTime();
await stdlib.wait(1);
await ctc.a.poke();
const t4 = await stdlib.getNetworkTime();
await stdlib.wait(1);

assertEq((await ctc.e.e.nextUpToTime(t3)).what, 2);
assertEq((await ctc.e.e.nextUpToTime(t3)).what, 3);
assertEq((await ctc.e.e.nextUpToTime(t3)).what, 4);
assertEq((await ctc.e.e.nextUpToTime(t3)).what, 5);
assertEq((await ctc.e.e.nextUpToTime(t3)), undefined);

assertEq((await ctc.e.e.nextUpToTime(t4)).what, 6);
assertEq((await ctc.e.e.nextUpToTime(t4)).what, 7);
assertEq((await ctc.e.e.nextUpToTime(t4)), undefined);

console.log("Done testing nextUpToTime.");
