import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import * as clientBackend from './build/index.client.mjs';
import { util } from '@reach-sh/stdlib';
const { thread, Signal } = util;
const stdlib = loadStdlib();

const startingBalance = stdlib.parseCurrency(100);

const conWait = 5000;

const goGo = async (ctcAlice, sig) => {
  const acc = (await stdlib.newTestAccount(startingBalance)).setDebugLabel('FE_API');
  return async () => {
    const ctc = acc.contract(backend, ctcAlice.getInfo());
    const go = ctc.a.go;
    const go2 = ctc.a.go2;
    await sig.wait();

    const call = async (id, f, exp) => {
      let res = undefined;
      await new Promise(resolve => setTimeout(resolve, conWait))
      try { res = await f(); }
      catch (e) { res = [`err`, e]; }
      console.log(id, res.toString());
      stdlib.assert(res.eq(exp));
    }

    await call('go 1:', () => go(1), 1);
    await call('go2 :', () => go2(3), 3);
    await call('go 2:', () => go(2), 2);
  }
}

const goClient = async (ctcAlice, sig) => {
  const acc = (await stdlib.newTestAccount(startingBalance)).setDebugLabel('CLI');
  acc.setGasLimit(5000000);
  return async () => {
    const ctc = acc.contract(clientBackend);
    await sig.wait();

    await Promise.all([
      clientBackend.A(ctc, {
        ctc: () => ctcAlice.getInfo(),
        done: () => console.log('done'),
      })
    ])
  }
}


const go = async (f) => {

  const [ accAlice ] =
    await stdlib.newTestAccounts(1, startingBalance);

  accAlice.setGasLimit(5000000);

  const ctcAlice = accAlice.contract(backend);

  const ready = new Signal();

  await Promise.all([
    thread(await f(ctcAlice, ready)),
    ctcAlice.p.Alice({
      deployed: async (_ctcInfo) => {
        console.log(`Deployed`);
        ready.notify();
      },
      done: () => {
        console.log('done')
      }
    }),
  ]);
}

await go(goGo);
await go(goClient);
