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

const [ accAlice ] =
  await stdlib.newTestAccounts(1, startingBalance);

const ctcAlice = accAlice.contract(backend);

const ready = new Signal();

const goGo = async () => {
  const acc = await stdlib.newTestAccount(startingBalance);
  return async () => {
    const ctc = acc.contract(backend, ctcAlice.getInfo());
    const go = ctc.a.go;
    await ready.wait();

    const call = async (id, f, exp) => {
      let res = undefined;
      try { res = await f(); }
      catch (e) { res = [`err`, e]; }
      console.log(id, res.toString());
      stdlib.assert(res.eq(exp));
    }

    await call('go 1:', () => go(1), 1);
    await call('go 2:', () => go(2), 2);
  }
}

await Promise.all([
  thread(await goGo()),
  ctcAlice.p.Alice({
    deployed: async () => {
      console.log(`Deployed`);
      ready.notify();
    }
  }),
]);

