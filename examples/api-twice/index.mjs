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

const n = 3;

const conWait = 5000;

const go = async (ctcAdmin, sig) => {
  const acc = (await stdlib.newTestAccount(startingBalance)).setDebugLabel('FE_API');
  return async () => {
    const ctc = acc.contract(backend, ctcAdmin.getInfo());
    const f = ctc.a.Writer.f;
    await sig.wait();

    const call = async (id, f, exp) => {
      let res = undefined;
      await new Promise(resolve => setTimeout(resolve, conWait))
      try { res = await f(); }
      catch (e) { res = [`err`, e]; }
      console.log(id);
      stdlib.assert(res.eq(exp));
    }

    for (let i = 0; i < n; i++) {
      await call(`first parallelReduce: ${i + 1} of ${n}`, () => f(), 1);
    }
    for (let i = 0; i < n; i++) {
      await call(`second parallelReduce: ${i + 1} of ${n}`, () => f(), 2);
    }
  }
}


(async () => {

  const [ accAdmin ] =
    await stdlib.newTestAccounts(1, startingBalance);

  accAdmin.setGasLimit(5000000);

  const ctcAdmin = accAdmin.contract(backend);

  const ready = new Signal();

  await Promise.all([
    thread(await go(ctcAdmin, ready)),
    ctcAdmin.p.Admin({
      deployed: async () => {
        console.log(`Deployed`);
        ready.notify();
      },
      n: stdlib.bigNumberify(n),
    }),
  ]);
})()