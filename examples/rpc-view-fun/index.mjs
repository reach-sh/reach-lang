import { mkRPC }      from '@reach-sh/rpc-client';
import { mkAssertEq } from './common.mjs';

  const { rpc, rpcCallbacks } = await mkRPC();

  const accAlice = await rpc('/stdlib/newTestAccount', await rpc('/stdlib/parseCurrency', 100));
  const ctcAlice = await rpc('/acc/contract', accAlice);

  const checkView = async (x, e) => mkAssertEq(rpc)(
    'checkView:', e, await rpc('/ctc/v/Main/f', ctcAlice, x));

  await rpcCallbacks('/backend/Alice', ctcAlice, { checkView });

  await Promise.all([
    rpc('/forget/ctc', ctcAlice),
    rpc('/forget/acc', accAlice),
  ]);
