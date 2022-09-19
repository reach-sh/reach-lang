import {loadStdlib} from '@reach-sh/stdlib';
import * as backendClient from './build/index.client.mjs';
import * as backendServer from './build/index.server.mjs';
const stdlib = loadStdlib(process.env);

const amount = stdlib.parseCurrency(50);
const accA = await stdlib.newTestAccount(stdlib.parseCurrency(100));
const accB = await stdlib.newTestAccount(0);

const ctcServer = accA.contract(backendServer);
const ctcClient = accA.contract(backendClient);

try {
  await ctcServer.p.Alice({
    setup: [await accB.getAddress()],
    ready: (ctc) => {
      throw 42;
    },
  });
} catch (e) {
  if ( e !== 42) {
    throw e;
  }
}

await ctcClient.p.Alice({
  setup: [await accB.getAddress(),
          amount,
          await ctcServer.getInfo()],
});

const bb = await accB.balanceOf();
if (bb < amount){
  throw `Got wrong balance, expected ${amount}, got ${bb}.`;
} else {
  console.log("\nTest passed successfully.\n");
}
