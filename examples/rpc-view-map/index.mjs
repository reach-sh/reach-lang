import { mkRPC }      from '@reach-sh/rpc-client';
import { mkAssertEq } from './common.mjs';

(async () => {
  const { rpc, rpcCallbacks } = await mkRPC();

  const startingBalance      = await rpc('/stdlib/parseCurrency', 100);
  const [ accAlice, accBob ] = await Promise.all([
    rpc('/stdlib/newTestAccount', startingBalance),
    rpc('/stdlib/newTestAccount', startingBalance),
  ]);

  const ctcAlice = await rpc('/acc/contract', accAlice);
  let ctcBob;

  await rpc('/acc/setDebugLabel', accAlice, 'Alice');
  await rpc('/acc/setDebugLabel', accBob,   'Bob');

  const checkView = async (x, who, e) => {
    const w = await rpc('/stdlib/formatAddress', who);
    mkAssertEq(rpc)(`checkView [${w}]:`, e, await rpc('/ctc/v/Main/f', ctcAlice, who));
  };

  await Promise.all([
    rpcCallbacks('/backend/Alice', ctcAlice, { checkView }),

    rpc('/ctc/getInfo', ctcAlice)
      .then(async i => {
        ctcBob = await rpc('/acc/attach', accBob, i);
        await rpcCallbacks('/backend/Bob', ctcBob, {});
      }),
  ]);

  await Promise.all([
    rpc('/forget/ctc', ctcAlice),
    rpc('/forget/acc', accAlice),

    rpc('/forget/ctc', ctcBob),
    rpc('/forget/acc', accBob),
  ]);
})();
