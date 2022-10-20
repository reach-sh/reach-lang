import {Reach} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const reach = new Reach();
const accD = await reach.newTestAccount(reach.parseCurrency(100));
const ctcD = await accD.contract(backend);
const ctcO = await reach.contract(backend, ctcD.getInfo());
const s = ['💩ABCD', '💩EFGH'];
await reach.withDisconnect(() => backend.D(ctcD, {
  s, ready: reach.disconnect
}));
const vs = await ctcO.v.s();
await ctcD.a.halt();
console.log({s, vs});
reach.assert(vs[0] === 'Some')
reach.assert(vs[1][0] === s[0]);
reach.assert(vs[1][1] === s[1]);
