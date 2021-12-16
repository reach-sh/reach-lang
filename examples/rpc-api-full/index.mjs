import { mkRPC }      from '@reach-sh/rpc-client';
import Timeout        from 'await-timeout';

export class Signal {
  constructor() { this.p = new Promise(res => { this.r = res; }); }
  wait()        { return this.p; }
  notify()      { this.r(true);  }
};

(async () => {
  const { rpc, rpcCallbacks } = await mkRPC();

  const amt_ = await rpc('/stdlib/parseCurrency', 2);
  const bal  = await rpc('/stdlib/parseCurrency', 100);

  const accAdmin = await rpc('/stdlib/newTestAccount', bal);
  await rpc('/acc/setDebugLabel', accAdmin, 'Admin');

  const gil = await rpc('/stdlib/launchToken', accAdmin, 'gil', 'GIL');

  const getBal = async w => Promise.all([
    rpc('/stdlib/formatCurrency', await rpc('/stdlib/balanceOf', w),            4),
    rpc('/stdlib/formatCurrency', await rpc('/stdlib/balanceOf', w, gil.token), 4),
  ]);

  const showBal = (who, acc) => async when =>
    console.log(who, when, await getBal(acc));

  const ctcAdmin = await rpc('/acc/contract', accAdmin);
  const accs     = [ accAdmin ];
  const ctcs     = [ ctcAdmin ];
  const ready    = new Signal();

  const user = async uid => {
    const a = await rpc('/stdlib/newTestAccount', bal);
    accs.push(a);
    await rpc('/acc/setDebugLabel', a, uid);

    await rpc('/acc/tokenAccept', a, gil.kid);
    await rpc('/launchToken/mint', gil.kid, a, bal);

    return async () => {
      const c = await rpc('/acc/contract', a, await rpc('/ctc/getInfo', ctcAdmin));
      ctcs.push(c);

      const getRead     = async () => rpc('/ctc/v/Reader/read', c);
      const showUserBal = showBal(uid, a);

      await ready.wait();
      await showUserBal('Start');

      let i = 0;
      const callPut = async (t, ...args) => {
        await showUserBal(`Sleep ${i}`);
        await Timeout.set((10 + i) * Math.random());
        await showUserBal(`Before ${i}`);
        const before = await getRead();

        let res;
        try {
          res = await rpc(`/ctc/a/Writer/${t}`, c, ...args);
        } catch (e) {
          res = [ 'err', e ];
        }

        const after = await getRead();
        console.log(uid, i, before, res, after);
        await showUserBal(`After ${i}`);
        i++;
      };

      await callPut('touch',  i);
      await callPut('writeN', i);
      await callPut('touch',  i);
      await callPut('writeN', i);
      await callPut('touch',  i);
      await callPut('writeT', i);
      await callPut('touch',  i);
      await callPut('writeB', i);
      await callPut('touch',  i);
      await callPut('end');

      await showUserBal('End');
    };
  };

  const showAdminBal = showBal('Admin', accAdmin);
  await showAdminBal('Start');

  const thread = async f => await f();
  await Promise.all([
    thread(await user('Alice')),
    thread(await user('Bob')),

    rpcCallbacks('/backend/Admin', ctcAdmin, {
      log: (...args) => {
        console.log(...args);
        ready.notify();
      },
      tok: gil.token.id,
      amt: amt_,
    }),
  ]);

  await showAdminBal('End');

  await Promise.all([
    ...ctcs.map(c => rpc('/forget/ctc', c)),
    ...accs.map(a => rpc('/forget/acc', a)),
    rpc('/forget/token', gil.kid),
  ]);
})();
