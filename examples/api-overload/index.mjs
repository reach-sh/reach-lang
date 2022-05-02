import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import * as clientBackend from './build/index.client.mjs';
import { util } from '@reach-sh/stdlib';
const { thread, Signal } = util;

const stdlib = loadStdlib(process.env);
const startingBalance = stdlib.parseCurrency(100);

const log = (lvl, s) => console.log(`${' '.repeat(lvl * 2)}${s}`);

const doFrontend = async () => {

  log(0, 'Frontend');
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

      log(1, 'Calling `mul1`');
      await call('mul1', () => mul1(10));
      log(1, 'Calling `mul2`');
      await call('mul2', () => mul2(10, 23));
    }
  }

  await Promise.all([
    backend.Alice(ctcAlice, {
      deployed: () => {
        log(1, 'Alice deployed contract.');
        ready.notify();
      },
      done: () => log(1, 'Contract finished'),
    }),
    thread(await user()),
  ]);
};

export const doC2C = async () => {
  log(0, `Contract 2 Contract`);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const ctcAlice = accAlice.contract(backend);
  const ready = new Signal();

  const bob = async () => {
    log(1, `Bob waits for deployment`)
    await ready.wait();
    log(1, `Bob is starting`)
    const accBob = await stdlib.newTestAccount(startingBalance);
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
        log(1, 'Alice deployed contract.');
        ready.notify();
      },
      done: () => log(1, 'Contract finished'),
    }),
    bob()
  ]);
}

export const main = async () => {
  await doFrontend();
  await doC2C();
};
main();
