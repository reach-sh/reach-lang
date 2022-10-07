import {Reach} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import { sha512_256 } from '@reach-sh/stdlib/node_modules/js-sha512/src/sha512.js';
const reach = new Reach();

const ctc = await reach.contract(backend);
const tys = ctc.getEventTys();
const p = tys && tys['pub'];
const b = tys && tys['big'];
reach.assert(p);
reach.assert(b);
reach.assert(p.length === 1);
reach.assert(b.length === 2);
reach.assert(p[0].name === 'UInt');
reach.assert(b[0].name.replace(/\s/g, '') === 'Tuple(Data(None:Null,Some:Bool),Array(Bytes(2),2))');
reach.assert(b[1].name.replace(/\s/g, '') === 'Tuple(Address,Contract,UInt,Digest)');
if (reach.connector === 'ALGO') {
  reach.assert(p[0].toString() === 'uint64');
  reach.assert(b[0].toString() === '((byte,byte[1]),byte[2][2])');
  reach.assert(b[1].toString() === '(address,uint64,uint256,digest)');
} else { // 'ETH' || 'CFX'
  reach.assert(p[0].toString() === 'uint256');
  reach.assert(b[0].toString() === 'tuple(tuple(uint256 which,bool _None,bool _Some),bytes2[2])');
  reach.assert(b[1].toString() === 'tuple(address,address,uint256,uint256)')
}
console.log(`static assertions passed`);

const amt = reach.parseCurrency('100');
const deployer = await reach.newTestAccount(amt);
const ctcD = deployer.contract(backend);
const i = 7;
await ctcD.p.Deployer({i});

// We can use the info from getEventSigs to observe events "externally".
// See ARC-0028. https://github.com/algorandfoundation/ARCs/pull/113
if (reach.connector === 'ALGO') {
  const appid = (await ctcD.getInfo()).toNumber();
  await ctcD.e.big.next(); // just to ensure the indexer is caught up
  const {indexer} = await reach.getProvider(); // This is a regular old algosdk Indexer
  const log_b64s_nested = await indexer.lookupApplicationLogs(appid).do();
  const log_b64s = log_b64s_nested['log-data'].map((x) => x.logs).flat();
  const logs = log_b64s.map((l) => Buffer.from(l, 'base64'));
  console.log('logs', logs);
  for (const e of ['pub', 'big']) {
    const sig = `${e}(${tys[e].toString()})`; // easy as that to construct the sig
    const prefix_hex = sha512_256(sig).slice(0,8);
    const explen = tys[e].reduce((s, t) => s + t.netSize, 4);
    console.log(prefix_hex, 'size', explen, 'from', sig);
    const elogs = logs.filter((b) => prefix_hex === b.slice(0,4).toString('hex'));
    reach.assert(elogs.length === 1);
    reach.assert(elogs[0].length == explen);
    if (e === 'pub') {
      const ihex = Buffer.from(reach.T_UInt.toNet(i)).toString('hex');
      reach.assert(elogs[0].toString('hex') === `${prefix_hex}${ihex}`);
    }
  }
  console.log(`log observation assertions passed`);
}
