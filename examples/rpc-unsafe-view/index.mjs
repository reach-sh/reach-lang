import { mkRPC }      from '@reach-sh/rpc-client';
import { mkAssertEq } from './common.mjs';

(async () => {
  const { rpc, rpcCallbacks } = await mkRPC();

  const assertEq = mkAssertEq(rpc);
  const accAlice = await rpc('/stdlib/newTestAccount', await rpc('/stdlib/parseCurrency', 100));
  const ctcAlice = await rpc('/acc/contract', accAlice);

  const observe = async () => {
    const e = [ await rpc('/stdlib/bigNumberify', 4), false ];
    const a = await rpc('/ctc/unsafeViews/t', ctcAlice);
    assertEq('Expecting `t` to have been set:', e, a);

    try {
      await rpc('/ctc/unsafeViews/u', ctcAlice);
      assertEq('Expecting exception was not thrown:', true, false);
    } catch (e1) {
      assertEq('Expecting exception was thrown:', true, true);

      try {
        assertEq('Expecting RPC server to continue functioning normally after exception was thrown:',
          [ "None", null ],
          await rpc('/ctc/views/u', ctcAlice));
      } catch (e2) {
        assertEq('Expecting exception was not thrown:', true, false);
      }
    }
  };

  await rpcCallbacks('/backend/Alice', ctcAlice, { observe });

  await Promise.all([
    rpc('/forget/ctc', ctcAlice),
    rpc('/forget/acc', accAlice),
  ]);
})();
