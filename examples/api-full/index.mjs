import { loadStdlib } from '@reach-sh/stdlib';
import Timeout from 'await-timeout';
import * as backend from './build/index.main.mjs';
const thread = async (f) => await f();

(async () => {
  const stdlib = await loadStdlib();
  const amt_ = stdlib.parseCurrency(2);
  const bal = stdlib.parseCurrency(10);
  const accAdmin = await stdlib.newTestAccount(bal);
  accAdmin.setDebugLabel('Admin');
  const gil = await stdlib.launchToken(accAdmin, "gil", "GIL");
  const getBal = async (who) =>
    [ stdlib.formatCurrency(await stdlib.balanceOf(who), 4),
      stdlib.formatCurrency(await stdlib.balanceOf(who, gil.id), 4) ]
  const showBal = (who, acc) => async (when) =>
    console.log(who, when, await getBal(acc));
  const ctcAdmin = accAdmin.deploy(backend);

  const user = async (uid) => {
    const acc = await stdlib.newTestAccount(bal);
    acc.setDebugLabel(uid);
    await acc.tokenAccept(gil.id);
    await gil.mint(acc, bal);
    return async () => {
      const ctc = acc.attach(backend, ctcAdmin.getInfo());
      const get = ctc.v.Reader;
      const put = ctc.a.Writer;
      const showUserBal = showBal(uid, acc);

      await showUserBal('start');
      let i = 0;
      const call = async (f) => {
        await showUserBal(`sleep ${i}`);
        await Timeout.set((10 + i) * Math.random());
        await showUserBal(`before ${i}`);
        const before = await get.read();
        const res = await f();
        const after = await get.read();
        console.log(uid, i, before, res, after);
        await showUserBal(`after ${i}`);
        i++;
      };

      await call(() => put.touch(i));
      await call(() => put.writeN(i));
      await call(() => put.touch(i));
      await call(() => put.writeN(i));
      await call(() => put.touch(i));
      await call(() => put.writeT(i));
      await call(() => put.touch(i));
      await call(() => put.writeB(i));
      await call(() => put.touch(i));
      await call(() => put.end());

      await showUserBal('end');
    };
  };

  const showAdminBal = showBal('Admin', accAdmin);
  await showAdminBal('start');
  await Promise.all([
    backend.Admin(ctcAdmin, {
      ...stdlib.hasConsoleLogger,
      tok: gil.id,
      amt: amt_,
    }),
    thread(await user('Alice')),
    thread(await user('Bob')),
  ]);
  await showAdminBal('end');
})();
