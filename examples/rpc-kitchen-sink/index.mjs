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

  /* Views */
  const checkViewFun = async (l, x, y, e) => {
    await assertEq(`ctc.v     > ${l}:`, e, await rpc(`/ctc/v/${l}`,     ctcAlice, x, y));
    await assertEq(`ctc.views > ${l}:`, e, await rpc(`/ctc/views/${l}`, ctcAlice, x, y));
  };

  const checkViewBytes = async (l1, l2, e) => {
    await assertEq(`ctc.v     > ${l1} + ${l2}:`, e, [ await rpc(`/ctc/v/${l1}`,     ctcAlice),
                                                      await rpc(`/ctc/v/${l2}`,     ctcAlice) ]);
    await assertEq(`ctc.views > ${l1} + ${l2}:`, e, [ await rpc(`/ctc/views/${l1}`, ctcAlice),
                                                      await rpc(`/ctc/views/${l2}`, ctcAlice) ]);
  };

  /* Exports */
  // TODO: how should we access nested elements in arrays?
  const five = await rpc('/backend/getExports/five');
  assertEq('backend.getExports > app.timesTwoPlusThree(app.five):',
    await rpc('/stdlib/bigNumberify', 13),
    await rpc('/backend/getExports/timesTwoPlusThree', five));

  assertEq('backend.getExports > app.a.b.e:',
    [ five, await rpc('/stdlib/bigNumberify', 6) ],
    await rpc('/backend/getExports/a/b/e'));

  assertEq('backend.getExports > app.a.b.c:',         'd',  await rpc('/backend/getExports/a/b/c'));
  assertEq('backend.getExports > app.a.z.c:',         null, await rpc('/backend/getExports/a/z/c'));
  assertEq('backend.getExports > app.nonExistent:',   null, await rpc('/backend/getExports/nonExistent'));
  assertEq('backend.getExports > app.nonExistent.a:', null, await rpc('/backend/getExports/nonExistent/a'));

  await rpcCallbacks('/backend/Alice', ctcAlice, { checkViewFun, checkViewBytes, meta });
})();
