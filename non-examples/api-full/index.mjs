import { loadStdlib } from '@reach-sh/stdlib';
import Timeout from 'await-timeout';
import * as backend from './build/index.main.mjs';
const thread = async (f) => await f();

(async () => {
  const stdlib = await loadStdlib();
  const amt_ = stdlib.parseCurrency(2);
  const bal = stdlib.parseCurrency(10);
  const accAdmin = await stdlib.newTestAccount(bal);
  const gil = await stdlib.launchToken(accAdmin, "gil", "GIL");
  const getBal = async (who) =>
    [ stdlib.formatCurrency(await stdlib.balanceOf(who), 4),
      stdlib.formatCurrency(await stdlib.balanceOf(who, gil.id), 4) ]
  const showBal = (who, acc) => async (when) =>
    console.log(who, when, await getBal(acc));
  const ctcAdmin = accAdmin.deploy(backend);

  const user = async (uid) => {
    const acc = await stdlib.newTestAccount(bal);
    await acc.tokenAccept(gil.id);
    await gil.mint(acc, bal);
    return async () => {
      const ctc = acc.attach(backend, ctcAdmin.getInfo());
      const get = ctc.getViews().Reader;
      const put = ctc.getAPIs().Write;
      const showUserBal = showBal(uid, acc);

      showUserBal('start');
      let i = 0;
      const call = async (f) => {
        showUserBal(`sleep ${i}`);
        await Timeout.set((10 + i) * Math.random());
        showUserBal(`before ${i}`);
        const before = await get.read();
        const res = await f();
        const after = await get.read();
        console.log(uid, i, before, res, after);
        showUserBal(`after ${i}`);
        i++;
      };

      await call(() => put.touch(i));
      await call(() => put.writeN.pay(amt_)(i));
      await call(() => put.touch(i));
      await call(() => put.writeN.pay([amt_])(i));
      await call(() => put.touch(i));
      await call(() => put.writeT.pay([[amt_, gil.id]])(i));
      await call(() => put.touch(i));
      await call(() => put.writeB.pay([amt_, [amt_, gil.id]])(i));
      await call(() => put.touch(i));
      await call(() => put.end());

      showUserBal('end');
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
