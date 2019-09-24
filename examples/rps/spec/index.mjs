import * as RPS        from '../build/rps.mjs';
import { stdlibNode  } from '@reach-sh/stdlib';
import { runGameWith } from '../index.mjs';

jasmine.DEFAULT_TIMEOUT_INTERVAL = 60 * 1000 * 10;

const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';

describe('A rock/paper/scissors game using the `web3` stdlib', () => {
  let stdlib;

  beforeAll(done => stdlibNode(uri)
    .then(l => { stdlib = l; })
    .then(done));

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

    const msgAgreeAndSaneBalances =
      [ 'both participants agreeing on who won'
      , 'and the winner\'s balance being increased'
      , '+ loser\'s balance being reduced by wager'
      ].join(' ');

    it(msgAgreeAndSaneBalances, done =>
      runGameWith(RPS, stdlib, false, false, interactWith, wagerInEth, escrowInEth)
        .then(g => {
          const { balanceStartAlice, balanceStartBob, balanceEndAlice, balanceEndBob } = g;

            expect(g.outcomeAlice)
                .toEqual(g.outcomeBob,
                         `outcomes disagree: (${g.outcomeAlice}) != (${g.outcomeBob})`);

          // "The Man" always gets his cut regardless - this is just a
          // rough guesstimate of processing fees
          const estimatedGas = stdlib.toBN(stdlib.toWei('5000000', 'wei'));

          if (g.outcomeAlice == 'Draw') {
              expect(balanceStartAlice.gte(balanceEndAlice.sub(estimatedGas)))
                  .toBe(true, `! alice > alice' - gas`);
              expect(balanceStartBob.gte(balanceEndBob.sub(estimatedGas)))
                  .toBe(true, `! bob > bob' - gas`);
          } else {

            const [ balStartWinner, balStartLoser, balEndWinner, balEndLoser ]
              = g.outcomeAlice == 'Alice wins'
                ? [ balanceStartAlice, balanceStartBob, balanceEndAlice, balanceEndBob ]
                : [ balanceStartBob, balanceStartAlice, balanceEndBob, balanceEndAlice ];

            const winnerGte = balEndWinner.gte(
              balStartWinner.add(g.wagerInWei)
                            .sub(estimatedGas));

            const loserLt = balEndLoser.lt(
              balStartLoser.sub(g.wagerInWei));

              expect(winnerGte).
                  toBe(true, [ `winner'(${balEndWinner}) >= winner(${balStartWinner}) +`
                             , `wager(${g.wagerInWei}) - gas(${estimatedGas})` ].join(' '));
              expect(loserLt  ).toBe(true, `loser' < loser - wager`);
          }

          done();
        }));
  });
});
