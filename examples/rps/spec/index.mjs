import * as RPS        from '../build/rps.mjs';
import { runGameWith } from '../index.mjs';

jasmine.DEFAULT_TIMEOUT_INTERVAL = 60 * 1000 * 10;

describe('RPS', () => {
  describe('results in', () => {
    const interactWith = (name, handf) =>
          ({ params: () => true,
             accepts: (wagerAmount, escrowAmount) => (void(wagerAmount, escrowAmount), true),
             getHand: () => handf(),
             commits: () => true,
             shows: () => true,
             reveals: (handB) => (void(handB), true),
             outcome: () => true });

    const wagerInEth  = '1.5';
    const escrowInEth = '0.15';

    it('both participants agreeing on who won and the winner\'s balance being increased + loser\'s balance being reduced by wager', done =>
       runGameWith(RPS, false, interactWith, wagerInEth, escrowInEth)
       .then(g => {
         const { balanceStartAlice, balanceStartBob, balanceEndAlice, balanceEndBob } = g;

         expect(g.outcomeAlice)
           .toEqual(g.outcomeBob,
                    `outcomes disagree: (${g.outcomeAlice}) != (${g.outcomeBob})`);

         if (g.outcomeAlice == 'Draw') {
           expect(balanceStartAlice.gte(balanceEndAlice))
             .toBe(true, `! alice > alice' - gas`);
           expect(balanceStartBob.gte(balanceEndBob))
             .toBe(true, `! bob > bob' - gas`);
         } else {
           const [ balStartWinner, balStartLoser, balEndWinner, balEndLoser ]
                 = g.outcomeAlice == 'Alice wins'
                 ? [ balanceStartAlice, balanceStartBob, balanceEndAlice, balanceEndBob ]
                 : [ balanceStartBob, balanceStartAlice, balanceEndBob, balanceEndAlice ];

           const winnerGte = balEndWinner.gte(balStartWinner.add(g.wagerInWei));

           const loserLt = balEndLoser.lt(balStartLoser.sub(g.wagerInWei));

           expect(winnerGte)
             .toBe(true, `winner'(${balEndWinner}) >= winner(${balStartWinner}) + wager(${g.wagerInWei})`);
           expect(loserLt)
             .toBe(true, `loser' < loser - wager`);
         }

         done();
       }));
  });
});
