import {Reach} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const reach = new Reach();
const ctc = await reach.contract(backend);
const e = ctc.getEventSigs();
console.log(e);
reach.assert(e.sigs.length === 1);
reach.assert(e.sigs[0] === 'pub(UInt)');
if (reach.connector === 'ALGO') {
  reach.assert(e.ALGO.length === 1);
  reach.assert(e.ALGO[0] === 'pub(uint64)');
}
console.log(`all assertions passed`);
