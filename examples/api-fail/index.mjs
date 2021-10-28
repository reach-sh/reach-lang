import { loadStdlib } from '@reach-sh/stdlib';
import Timeout from 'await-timeout';
import * as backend from './build/index.main.mjs';
const thread = async (f) => await f();

export class Signal {
  constructor() {
    const me = this;
    this.p = new Promise((resolve) => { me.r = resolve; });
  }
  wait() { return this.p; }
  notify() { this.r(true); }
};

(async () => {
  const stdlib = loadStdlib(process.env);
  const amt = stdlib.parseCurrency(2);
  const bal = stdlib.parseCurrency(100);
  const [ accAdmin, accUser ] = await stdlib.newTestAccounts(2, bal);
  accAdmin.setDebugLabel('Admin');
  accUser.setDebugLabel('User');
  const ctcAdmin = accAdmin.contract(backend);
  const launched = new Signal();
  const returnAmt = new Signal();
  const ready = new Signal();
  const done = new Signal();
  const ctcUser = accUser.contract(backend, ctcAdmin.getInfo());

  const inform = console.log;

  await Promise.all([
    thread(async () => {
      const doInform = (...pre) => (...post) => {
        inform('Admin', ...pre, ...post);
      };
      const notifyInform = (sig, when, ...more) => () => {
        doInform(`Signal ${when}`, ...more)();
        sig.notify();
      };
      await backend.Admin(ctcAdmin, {
        launched: notifyInform(launched, 'Launched'),
        getAmt: (async () => {
          doInform('Wait for returnAmt');
          await returnAmt.wait();
          doInform('returnAmt');
          return amt;
        }),
        ready: notifyInform(ready, 'Ready'),
        inform: doInform('Inform'),
        done: notifyInform(done, 'Done'),
      });
    }),
    thread(async () => {
      const U = ctcUser.a.User;
      const doInform = (...x) => inform('User', ...x);
      const err = async (pf) => {
        try {
          return [ 'ok', await pf() ];
        } catch (e) {
          return [ 'err', e ];
        }
      };

      doInform('Start');
      doInform('Wait for Launched');
      await launched.wait();
      doInform('Launched');
      doInform('Failed call to read', await err(() => U.read()));
      doInform('Signal returnAmt');
      returnAmt.notify();
      doInform('Wait for Ready');
      await ready.wait();
      doInform('Ready');
      doInform('Succ call to read', await U.read());
      doInform('Succ call to pay', await U.pay());
      doInform('Succ call to write', await U.write(5));
      doInform('Succ call to write', await U.write(10));
      doInform('Failed call to writeC', await err(() => U.writeC(5)));
      doInform('Succ call to write', await U.write(5));
      doInform('Call Stop', await U.stop());
      doInform('Wait for Done');
      await done.wait();
      doInform('Failed call to read', await err(() => U.read()));
      doInform('Done');
    }),
  ]);
})();
