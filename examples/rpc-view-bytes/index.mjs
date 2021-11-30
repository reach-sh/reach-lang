import { mkRPC }      from '@reach-sh/rpc-client';
import { mkAssertEq } from './common.mjs';

(async () => {
  const { rpc, rpcCallbacks } = await mkRPC();

  const accAlice = await rpc('/stdlib/newTestAccount', await rpc('/stdlib/parseCurrency', 100));
  const ctcAlice = await rpc('/acc/contract', accAlice);
  const meta     = 'This is a test string';

  const checkView = async e => mkAssertEq(rpc)('Expected:', e, [
    await rpc('/ctc/v/Main/who',  ctcAlice),
    await rpc('/ctc/v/Main/meta', ctcAlice),
  ]);

  await rpcCallbacks('/backend/Alice', ctcAlice, { meta, checkView });
})();
