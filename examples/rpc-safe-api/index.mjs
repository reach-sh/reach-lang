import { mkRPC }      from '@reach-sh/rpc-client';
import { mkAssertEq } from './common.mjs';

(async () => {
  const { rpc, rpcCallbacks } = await mkRPC();

  const sbal     = await rpc('/stdlib/parseCurrency', 100);
  const accAlice = await rpc('/stdlib/newTestAccount', sbal);
  const ctcAlice = await rpc('/acc/contract', accAlice);
  const accs     = [ accAlice ];
  const ctcs     = [ ctcAlice ];

  const user = async (uid, exp) => {
    const a = await rpc('/stdlib/newTestAccount', sbal);
    accs.push(a);
    await rpc('/acc/setDebugLabel', a, uid);

    const i = await rpc('/ctc/getInfo', ctcAlice);
    const c = await rpc('/acc/contract', a, i);
    ctcs.push(c);

    const res = await rpc('/ctc/safeApis/go', c);
    await mkAssertEq(rpc)(`With ${uid}:`, exp, res);
  };

  await Promise.all([
    rpcCallbacks('/backend/Alice', ctcAlice, {
      go: async () => {
        await user('Bob', [ 'None', null ]);
        return false;
      },
    }),

    user('Carlos', [ 'Some', true ]),
  ]);

  await Promise.all([
    ...ctcs.map(c => rpc('/forget/ctc', c)),
    ...accs.map(a => rpc('/forget/acc', a)),
  ]);
})();
