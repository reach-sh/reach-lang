import { loadStdlib } from "@reach-sh/stdlib";
import * as backend from "./build/index.main.mjs";
const stdlib = loadStdlib();
const amount = stdlib.parseCurrency(100);
const creatorAcc = await stdlib.newTestAccount(amount.mul(2));
const creatorCtc = creatorAcc.contract(backend);
const curTime = await stdlib.getNetworkTime();
const duration = 50;
const start = curTime.add(20);
const end = curTime.add(duration);
await stdlib.withDisconnect(async () =>
  await creatorCtc.p.Creator({
    params: { amount, duration, start, },
    deployed: stdlib.disconnect,
  })
);
console.log("Starting queries...");
let total = stdlib.bigNumberify(0);
let moment = curTime;
while (moment.lt(end)) {
  await stdlib.waitUntilTime(moment);
  moment = moment.add(10);
  try {
    const [ released, resTime ] = await creatorCtc.a.release();
    total = total.add(released);
    console.log('WAITED', { start, end, amount, total, moment, released, resTime });
  } catch (e) {
    console.log("CAUGHT", e);
  }
}
