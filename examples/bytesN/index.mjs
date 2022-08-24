import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import { util } from '@reach-sh/stdlib';
const { thread, Signal, Timeout } = util;

const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);

const [ accAlice ] =
  await stdlib.newTestAccounts(1, startingBalance);

const ctcAlice = accAlice.contract(backend);

const ready = new Signal();

if (stdlib.connector == 'ETH') {
  const abi = ctcAlice.getABI();
  const f = abi.find(x => x.name == 'f');
  stdlib.assert(f.inputs[0].type == 'bytes4');
}

let i = 0;
await Promise.all([
  backend.Alice(ctcAlice, {
    log: console.log,
    deployed: () => {
      ready.notify();
    },
    s: "abcd",
    getS: () => {
      i++;
      return (i % 2 == 0) ? "efgh" : "ijkl";
    },
    chkViews: async (i) => {
      const { b32, b33 } = ctcAlice.unsafeViews;
      const b32r = await b32();
      stdlib.assert(b32r.length == 32);
      const b33r = await b33();
      stdlib.assert(b33r.length == 33);
    }
  }),
  thread(async () => {
    await ready.wait();
    const f = ctcAlice.a.f;
    const res = await f("abcd");
    stdlib.assert(res);
  })
]);
