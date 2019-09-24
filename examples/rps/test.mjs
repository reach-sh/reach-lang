import * as RPS        from './build/rps.mjs';
import { runGameWith } from './index.mjs';
import { runTests, assert } from '@reach-sh/stdlib/tester.mjs';

const interactWithAlice =
      ({ params: () => true,
         getHand: () => 'SCISSORS',
         commits: () => true,
         reveals: (handB) => (void(handB), true),
         outcome: () => true });
const interactWithBob =
      ({ accepts: (wagerAmount, escrowAmount) => (void(wagerAmount, escrowAmount), true),
         getHand: () => 'PAPER',
         shows: () => true,
         outcome: () => true });
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
  const winnerGte = balEndWinner.gte(balStartWinner.add(g.wagerInWei));
  const loserLt = balEndLoser.lt(balStartLoser.sub(g.wagerInWei));

  assert.ok(winnerGte, `winner'(${balEndWinner}) >= winner(${balStartWinner}) + wager(${g.wagerInWei})`);
  assert.ok(loserLt, `loser' < loser - wager`);
});
