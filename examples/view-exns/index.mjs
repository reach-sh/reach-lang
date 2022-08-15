// Behold, two dapps with identical ABIs,
// but differing behaviors.
import * as dapp_plus from './build/index.dapp_plus.mjs';
import * as dapp_sub from './build/index.dapp_sub.mjs';
import * as reachsdk from '@reach-sh/stdlib';
const stdlib = reachsdk.loadStdlib();
const amt = stdlib.parseCurrency('100');
const [ deployer, observer ] = await stdlib.newTestAccounts(2, amt);
const plusD = deployer.contract(dapp_plus);
const subD = deployer.contract(dapp_sub);
for (const {ctc, lab, argL, argR} of [
  {ctc: plusD, lab: 'plus', argL: 1, argR: 1},
  {ctc: subD,  lab: 'sub',  argL: 2, argR: 2},
]) {
  console.log(`${lab}: start...`);
  await stdlib.withDisconnect(async () => {
    await ctc.p.D({
      argL, argR, ready: stdlib.disconnect,
    });
  });
  console.log(`${lab}: ready`);
}
const plusInfo = await plusD.getInfo();
const subInfo = await subD.getInfo();
// Views on the corresponding contracts work correctly.
const resP = await observer.contract(dapp_plus, plusInfo).v.args();
console.log(`plus: view: ${JSON.stringify(resP)}`);
const resS = await observer.contract(dapp_sub, subInfo).v.args();
console.log(`sub: view: ${JSON.stringify(resS)}`);
// Views on the wrong contract throw an exn
const expectFail = async (p) => {
  try {
    const res = await p;
    console.error('Fail; this should have been unreachable');
    console.error(res);
    process.exit(1);
  } catch (e) {
    console.log(`failed successfully: ${e.toString().slice(0,62)}...`);
  }
};
expectFail(observer.contract(dapp_sub, plusInfo).v.args());
expectFail(observer.contract(dapp_plus, subInfo).v.args());
// let the ctcs end
await plusD.a.go();
await subD.a.go();
