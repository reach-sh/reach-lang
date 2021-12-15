import { mkRPC }      from '@reach-sh/rpc-client';
import { mkAssertEq } from './common.mjs';

(async () => {
  const { rpc, rpcCallbacks } = await mkRPC();

  const now      = await rpc('/stdlib/getNetworkTime');
  const startBal = await rpc('/stdlib/parseCurrency', 200);
  const accAlice = await rpc('/stdlib/newTestAccount', startBal);
  const accBob   = await rpc('/stdlib/newTestAccount', startBal);
  const ctcs     = [];

  await rpc('/stdlib/setQueryLowerBound', now);

  const run = async (p, whosBob) => {
    const ctcAlice = await rpc('/acc/contract', accAlice);
    ctcs.push(ctcAlice);

    const checkView = async e => mkAssertEq(rpc)(`Run w/ ${p} checkView:`, e, [
        await rpc('/ctc/v/Main/last', ctcAlice),
        await rpc('/ctc/v/Main/i',    ctcAlice),
      ]);

    await Promise.all([
      rpcCallbacks('/ctc/p/Alice', ctcAlice, { checkView }),

      rpc('/ctc/getInfo', ctcAlice)
        .then(async i => {
          const ctcBob = await rpc('/acc/contract', whosBob, i);
          ctcs.push(ctcBob);
          await rpcCallbacks('/ctc/p/Bob', ctcBob, {});
        }),
    ]);
  };

  await run('Bob',   accBob);
  await run('Alice', accAlice);

  await Promise.all([
    rpc('/forget/ctc', ...ctcs),
    rpc('/forget/acc', accAlice, accBob),
  ]);
})();
