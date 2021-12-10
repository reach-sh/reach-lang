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

  assertEq('With a 0-indexed element of a nested array (one level deep):',
    await b(1),
    await rpc('/backend/getExports/a/0'));

  assertEq('With a 0-indexed element of a nested array (three levels deep):',
    await b(6),
    await rpc('/backend/getExports/a/2/2/1'));

  assertEq('With an object of varying fields:',
    { a: await b(5), b: { c: true, d: await Promise.all([ b(1), b(2), [ true, false ]]) }},
    await rpc('/backend/getExports/o'));

  assertEq('With a sub-field of an object (one level deep):',
    { c: true, d: await Promise.all([ b(1), b(2), [ true, false ]]) },
    await rpc('/backend/getExports/o/b'));

  assertEq('With a sub-field of an object (two levels deep):',
    await Promise.all([ b(1), b(2), [ true, false ]]),
    await rpc('/backend/getExports/o/b/d'));

  assertEq('With `add1(1)`:',
    await b(2),
    await rpc('/backend/getExports/add1', await b(1)));

  assertEq('With `sumMul2(1, 2)`:',
    await b(6),
    await rpc('/backend/getExports/sumMul2', await b(1), await b(2)));

  assertEq('With a non-existent export:',
    null,
    await rpc('/backend/getExports/nonExistent'));

  assertEq('With a field of a non-existent export:',
    null,
    await rpc('/backend/getExports/nonExistent/foo'));

  assertEq('With a sub-field of a non-existent export (one level deep):',
    null,
    await rpc('/backend/getExports/nonExistent/foo/bar'));

  assertEq('With a non-existent index of an array:',
    null,
    await rpc('/backend/getExports/a/9'));

  assertEq('With a non-existent index of an array nested within an object:',
    null,
    await rpc('/backend/getExports/o/b/d/13'));

  assertEq('With a non-existent, nested index of an array (two levels deep):',
    null,
    await rpc('/backend/getExports/a/9/3'));

  assertEq('With a non-existent, nested index of an array nested within an object (four levels deep):',
    null,
    await rpc('/backend/getExports/o/b/d/13/6/7/8'));

  await rpcCallbacks('/backend/Alice', ctcAlice, { 'stdlib.hasRandom': true, x: 1, y: 2, });

  await Promise.all([
    rpc('/forget/ctc', ctcAlice),
    rpc('/forget/acc', accAlice),
  ]);
})();
