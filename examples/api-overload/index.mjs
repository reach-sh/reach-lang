import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import * as clientBackend from './build/client.main.mjs';

const stdlib = loadStdlib(process.env);
const startingBalance = stdlib.parseCurrency(100);

const thread = async (f) => await f();

export class Signal {
  constructor() {
    const me = this;
    this.p = new Promise((resolve) => { me.r = resolve; });
  }
  wait() { return this.p; }
  notify() { this.r(true); }
};

const doFrontend = async () => {

  const accAlice = await stdlib.newTestAccount(startingBalance);
  const ctcAlice = accAlice.contract(backend);

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
};

export const doC2C = async () => {
  console.log(`doC2C`);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const ctcAlice = accAlice.contract(backend);
  const ready = new Signal();

  const bob = async () => {
    console.log(`Waiting for deployment`)
    await ready.wait();
    console.log(`Starting Bob`)
    const accBob = accAlice; // await stdlib.newTestAccount(startingBalance);
    const ctcBob = accBob.contract(clientBackend);

    await Promise.all([
      clientBackend.A(ctcBob, {
        getCtc: async () => { return await ctcAlice.getInfo() },
      }),
    ]);
  }

  await Promise.all([
    backend.Alice(ctcAlice, {
      deployed: () => {
        console.log(`Alice: Deployed. Notifying`);
        ready.notify();
      }
    }),
    bob()
  ]);
}

export const main = async () => {
  // await doFrontend();
  await doC2C();
};
main();
