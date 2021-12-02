import { mkRPC }      from '@reach-sh/rpc-client';
import { mkAssertEq } from './common.mjs';

(async () => {
  const { rpc, rpcCallbacks } = await mkRPC();

  const accAlice = await rpc('/stdlib/newTestAccount', await rpc('/stdlib/parseCurrency', 100));
  const ctcAlice = await rpc('/acc/contract', accAlice);
  const assertEq = mkAssertEq(rpc);
  const b        = x => rpc('/stdlib/bigNumberify', x);

  assertEq('With nested arrays:',
    await Promise.all([
      b(1),
      b(2),
      Promise.all([
        b(3),
        b(4),
        Promise.all([ b(5), b(6), b(7), ]),
      ]),
    ]),
    await rpc('/backend/getExports/a'));

  assertEq('With an object of varying fields:',
    { a: await b(5), b: { c: true, d: await Promise.all([ b(1), b(2), [ true, false ]]) }},
    await rpc('/backend/getExports/o'));

  assertEq('With `add1(1)`:',
    await b(2),
    await rpc('/backend/getExports/add1', await b(1)));

  assertEq('With `sumMul2(1, 2)`:',
    await b(6),
    await rpc('/backend/getExports/sumMul2', await b(1), await b(2)));

  await rpcCallbacks('/backend/Alice', ctcAlice, { 'stdlib.hasRandom': true, x: 1, y: 2, });

  await Promise.all([
    rpc('/forget/ctc', ctcAlice),
    rpc('/forget/acc', accAlice),
  ]);
})();
