import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startTime = await stdlib.getNetworkTime();
const accAlice = await stdlib.newTestAccount(stdlib.parseCurrency(100));
const ctcAlice = accAlice.contract(backend);
await ctcAlice.p.Alice({
  giveTime: async (time) => {
    console.log(`The contract was launched at network time: ${time}`);
    console.log(`which is network seconds: ${await stdlib.getTimeSecs(time)}`);
  }    
});
console.log(`The program started at network time ${startTime}`);
console.log(`which is network seconds: ${await stdlib.getTimeSecs(startTime)}`);
await stdlib.wait(100);
const endTime = await stdlib.getNetworkTime();
console.log(`The program ended at ${endTime}`);
console.log(`which is network seconds: ${await stdlib.getTimeSecs(endTime)}`);
