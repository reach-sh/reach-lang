import * as RPS        from '../build/rps.mjs';
import { runGameWith } from '../index.mjs';

jasmine.DEFAULT_TIMEOUT_INTERVAL = 60 * 1000 * 10;

describe('RPS', () => {
  describe('results in', () => {
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

    it('outcome sane', done =>
       runGameWith(RPS, interactWithAlice, interactWithBob, wagerInEth, escrowInEth)
       .then(g => {
         const { balanceStartAlice, balanceStartBob, balanceEndAlice, balanceEndBob } = g;

         expect(g.outcomeAlice)
           .toEqual(g.outcomeBob,
                    `outcomes disagree: (${g.outcomeAlice}) != (${g.outcomeBob})`);

         expect(g.outcomeAlice)
           .toEqual(['Alice wins']);

         const [ balStartWinner, balStartLoser, balEndWinner, balEndLoser ] =
               [ balanceStartAlice, balanceStartBob, balanceEndAlice, balanceEndBob ];
         const winnerGte = balEndWinner.gte(balStartWinner.add(g.wagerInWei));
         const loserLt = balEndLoser.lt(balStartLoser.sub(g.wagerInWei));

         expect(winnerGte)
           .toBe(true, `winner'(${balEndWinner}) >= winner(${balStartWinner}) + wager(${g.wagerInWei})`);
         expect(loserLt)
           .toBe(true, `loser' < loser - wager`);

         done(); })); }); });
