import { mkRPC }      from '@reach-sh/rpc-client';
import { mkAssertEq } from './common.mjs';
import chalk          from 'chalk';

  const { rpc } = await mkRPC();

  const startingBalance      = await rpc('/stdlib/parseCurrency', 100);
  const [ accAlice, accBob ] = await Promise.all([
    rpc('/stdlib/newTestAccount', startingBalance),
    rpc('/stdlib/newTestAccount', startingBalance),
  ]);

  const lAlice = await rpc('/acc/setDebugLabel', accAlice, 'Alice');
  const lBob   = await rpc('/acc/setDebugLabel', accBob,   'Bob');

  const assertEq = mkAssertEq(rpc);

  console.log(chalk.blue.bold(
    [ '`acc.setDebugLabel` is designed as a fluent interface. Instead of a'
    , 'serialized `Account` representation we should expect the same account'
    , 'continuation ID we supplied back.'
    ].join(' ')));

  assertEq(`/acc/setDebugLabel [ accAlice, 'Alice' ]`, accAlice, lAlice);
  assertEq(`/acc/setDebugLabel [ accBob, 'Bob' ]`,     accBob,   lBob);

  rpc('/forget/acc', accAlice, accBob);
