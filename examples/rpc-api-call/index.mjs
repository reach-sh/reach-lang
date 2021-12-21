import { mkRPC }      from '@reach-sh/rpc-client';
import { mkAssertEq } from './common.mjs';

(async () => {
  const { rpc, rpcCallbacks } = await mkRPC();

  const sbal         = await rpc('/stdlib/parseCurrency', 100);
  const [ accAlice ] = await rpc('/stdlib/newTestAccounts', 1, sbal);
  const ctcAlice     = await rpc('/acc/contract', accAlice);
  const accs         = [ accAlice ];
  const ctcs         = [ ctcAlice ];
  const assertEq     = mkAssertEq(rpc);

  const user = async (uid, exp) => {
    const a = await rpc('/stdlib/newTestAccount', sbal);
    accs.push(a);
    await rpc('/acc/setDebugLabel', a, uid);

    const i = await rpc('/ctc/getInfo', ctcAlice);
    const c = await rpc('/acc/contract', a, i);
    ctcs.push(c);

    const call = async (a, args, e) => {
      const u = `/ctc/a/${a}`;
      const l = `RPC ${u}${args.length > 0 ? ' (' + args.join(', ') + ')' : ''}:`
      try {
        await assertEq(l, e, await rpc(u, c, ...args));
      } catch (x) {
        await assertEq(l, 'No exceptions to be thrown', x);
      }
    };

    await call('Bob/checkEq', [ 0,  0, ], true);
    await call('Bob/payMe',   [    10, ], await rpc('/stdlib/bigNumberify', 0));
    await call('Bob/noop',    [        ], false);
  };

  await Promise.all([
    rpcCallbacks('/backend/Alice', ctcAlice, {}),
    user('Bob'),
  ]);

  await Promise.all([
    ...ctcs.map(c => rpc('/forget/ctc', c)),
    ...accs.map(a => rpc('/forget/acc', a)),
  ]);
})();
