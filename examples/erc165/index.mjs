import { loadStdlib } from "@reach-sh/stdlib";
import * as backend from './build/index.main.mjs';
import { util } from '@reach-sh/stdlib';
const { thread, Signal } = util;

const stdlib = loadStdlib(process.env);

if (stdlib.connector !== "ETH") {
  console.log("This test only applies to ETH");
  process.exit(0);
}

const bal = stdlib.parseCurrency(100);
const [ accD ] = await stdlib.newTestAccounts(1, bal);
accD.setDebugLabel('Deployer');
const ctcD = accD.contract(backend);
const ready = new Signal();

await Promise.all([
  backend.Deployer(ctcD, {
    deployed: () => {
      ready.notify();
    }
  }),
  thread(async () => {
    const f = ctcD.a.supportsInterface;
    await ready.wait();

    const r1 = await f(stdlib.bytesFromHex('0x01ffc9a7'));
    console.log(`contract supports interface id 0x01ffc9a7:`, r1);
    stdlib.assert(r1);

    const r2 = await f(stdlib.bytesFromHex('0xffffffff'));
    console.log(`contract does not support interface id 0xffffffff:`, r2);
    stdlib.assert(!r2);

    process.exit();
  })
]);

