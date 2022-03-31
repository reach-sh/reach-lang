import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);

const accAlice = await stdlib.newTestAccount(startingBalance);

const ctcAlice = accAlice.contract(backend);

const thread = async (f) => await f();

export class Signal {
  constructor() {
    const me = this;
    this.p = new Promise((resolve) => { me.r = resolve; });
  }
  wait() { return this.p; }
  notify() { this.r(true); }
};

const ready = new Signal();

const user = async () => {
  const acc = await stdlib.newTestAccount(startingBalance);
  return async () => {
    const ctc = acc.contract(backend, ctcAlice.getInfo());
    const mul1 = ctc.a.mul1;
    const mul2 = ctc.a.mul2;
    await ready.wait();

    const call = async (id, f) => {
      let res = undefined;
      try { res = await f(); }
      catch (e) { res = [`err`, e]; }
      console.log(id, res.toString());
    }

    await call('mul1', () => mul1(10));
    await call('mul2', () => mul2(10, 23));
  }
}

await Promise.all([
  backend.Alice(ctcAlice, {
    deployed: () => {
      ready.notify();
    }
  }),
  thread(await user()),
]);
