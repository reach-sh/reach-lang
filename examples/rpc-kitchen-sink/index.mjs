import { mkRPC } from '@reach-sh/rpc-client';

(async () => {
  const { rpc, rpcCallbacks } = await mkRPC();

  const assertEq = async (l, e, a) => {
    const f = x => JSON.stringify(x).replace(/\\u0000/g, '');
    const [ ej, aj ] = [ f(e), f(a) ];
    if (ej === aj) {
      console.log(`${l} ${ej} === ${aj}`);
    } else {
      console.log(`${l} *** Mismatch! ***`);
      console.log(`  Expected: ${ej}`);
      console.log(`    Actual: ${aj}`);
    }
    await rpc('/stdlib/assert', ej === aj);
  };

  const accAlice = await rpc('/stdlib/newTestAccount', await rpc('/stdlib/parseCurrency', 100));
  const ctcAlice = await rpc('/acc/contract', accAlice);
  const meta     = "This is a test string";

  const checkViewFun = async (l, x, e) => {
    await assertEq(`ctc.v     > ${l}:`, e, await rpc(`/ctc/v/${l}`,     ctcAlice, x));
    await assertEq(`ctc.views > ${l}:`, e, await rpc(`/ctc/views/${l}`, ctcAlice, x));
  };

  const checkViewBytes = async (l1, l2, e) => {
    await assertEq(`ctc.v     > ${l1} + ${l2}:`, e, [ await rpc(`/ctc/v/${l1}`,     ctcAlice),
                                                      await rpc(`/ctc/v/${l2}`,     ctcAlice) ]);
    await assertEq(`ctc.views > ${l1} + ${l2}:`, e, [ await rpc(`/ctc/views/${l1}`, ctcAlice),
                                                      await rpc(`/ctc/views/${l2}`, ctcAlice) ]);
  };

  await rpcCallbacks('/backend/Alice', ctcAlice, { checkViewFun, checkViewBytes, meta });
})();
