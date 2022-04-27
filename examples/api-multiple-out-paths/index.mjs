import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();
const thread = async (f) => await f();

export class Signal {
  constructor() {
    const me = this;
    this.p = new Promise((resolve) => { me.r = resolve; });
  }
  wait() { return this.p; }
  notify() { this.r(true); }
};

const startingBalance = stdlib.parseCurrency(100);

const conWait = 5000;

const goGo = async (x, ctcAlice, ready) => {
  const acc = (await stdlib.newTestAccount(startingBalance)).setDebugLabel('FE_API');
  return async () => {
    const ctc = acc.contract(backend, ctcAlice.getInfo());
    const go = ctc.a.go;
    await ready.wait();

    const call = async (id, f, exp) => {
      let res = undefined;
      await new Promise(resolve => setTimeout(resolve, conWait))
      try { res = await f(); }
      catch (e) { res = [`err`, e]; }
      console.log(id, res);
      stdlib.assert(res == exp);
    }

    await call(`i > 5 | i == ${x} :`, () => go(x), x > 5);
  }
}

const main = async (x) => {

  const [ accAlice ] =
    await stdlib.newTestAccounts(1, startingBalance);

  accAlice.setGasLimit(5000000);

  const ctcAlice = accAlice.contract(backend);

  const ready = new Signal();

  await Promise.all([
    thread(await goGo(x, ctcAlice, ready)),
    ctcAlice.p.Alice({
      deployed: async (_ctcInfo) => {
        console.log(`Deployed`);
        ready.notify();
      },
      log: console.log,
    }),
  ]);
}

await main(1);
await main(6);