import * as RPS        from './build/rps.mjs';
import { runGameWith } from './index.mjs';
import { runTests, assert } from '@reach-sh/stdlib/tester.mjs';

const log = (msg, ret = true) => () => { console.log(`...${msg}`); return ret; };
const interactWithAlice =
      ({ params: log(`params`),
         getHand: log(`Alice getHand`, 'SCISSORS'),
         commits: log(`commits`),
         reveals: (handB) => log(`reveals ${handB}`)(),
         outcome: log(`Alice outcome`) });
const interactWithBob =
      ({ accepts: (wagerAmount, escrowAmount) => log(`accepts ${wagerAmount} ${escrowAmount}`)(),
         getHand: log(`Bob getHand`, 'PAPER'),
         shows: log(`shows`),
         outcome: log(`Bob outcome`) });
const wagerInEth  = '1.5';
const escrowInEth = '0.15';

runTests(async () => {
  console.log(`Running game...`);
  const g = await runGameWith(RPS, interactWithAlice, interactWithBob, wagerInEth, escrowInEth);

  const { balanceStartAlice, balanceStartBob, balanceEndAlice, balanceEndBob } = g;

  assert.deepEqual(g.outcomeAlice, g.outcomeBob,
                   `outcomes disagree: (${g.outcomeAlice}) != (${g.outcomeBob})`);

  assert.deepEqual(g.outcomeAlice, ['Alice wins']);

  const [ balStartWinner, balStartLoser, balEndWinner, balEndLoser ] =
        [ balanceStartAlice, balanceStartBob, balanceEndAlice, balanceEndBob ];

  assert.ok(balEndWinner.gte(balStartWinner), `winner'(${balEndWinner}) >= winner(${balStartWinner}) + wager(${g.wagerInWei})`);
  assert.ok(balEndLoser.lt(balStartLoser), `loser' < loser - wager`);
});
