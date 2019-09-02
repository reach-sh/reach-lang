// vim: filetype=javascript
jasmine.DEFAULT_TIMEOUT_INTERVAL = 60 * 1000 * 10;

import * as RPS        from '../../../../build/rps.mjs';
import { stdlibNode  } from '../../../stdlib/web3/node.mjs';
import { runGameWith } from '../../../demo.mjs';


const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';


describe('A rock/paper/scissors game using the `web3` stdlib', () => {
  let stdlib;

  beforeAll(done => stdlibNode(RPS.ABI, RPS.Bytecode, uri)
    .then(l => { stdlib = l; })
    .then(done));

  describe('results in', () => {
    const interactWith = (name, handf) => (a, cb) =>
      cb(a === 'getHand' ? handf() : null);

    const wagerInEth  = '1.5';
    const escrowInEth = '0.15';

    const msgAgreeAndSaneBalances =
      [ 'both participants agreeing on who won'
      , 'and the winner\'s balance being increased'
      , '+ loser\'s balance being reduced by wager'
      ].join(' ');

    it(msgAgreeAndSaneBalances, done =>
      runGameWith(RPS, stdlib, false, false, interactWith, wagerInEth, escrowInEth, uri)
        .then(g => {
          const { balanceStartAlice, balanceStartBob, balanceEndAlice, balanceEndBob } = g;

          expect(g.outcomeAlice === g.outcomeBob).toBe(true);

          // "The Man" always gets his cut regardless - this is just a
          // rough guesstimate of processing fees
          const estimatedGas = stdlib.toBN(stdlib.toWei('5000000', 'wei'));

          if (g.outcomeAlice === 'Draw') {
            expect(balanceStartAlice.gte(balanceEndAlice.sub(estimatedGas))).toBe(true);
            expect(balanceStartBob.gte(balanceEndBob.sub(estimatedGas))).toBe(true);
          } else {

            const [ balStartWinner, balStartLoser, balEndWinner, balEndLoser ]
              = g.outcomeAlice === 'Alice wins'
                ? [ balanceStartAlice, balanceStartBob, balanceEndAlice, balanceEndBob ]
                : [ balanceStartBob, balanceStartAlice, balanceEndBob, balanceEndAlice ];

            const winnerGte = balEndWinner.gte(
              balStartWinner.add(g.wagerInWei)
                            .sub(estimatedGas));

            const loserLt = balEndLoser.lt(
              balStartLoser.sub(g.wagerInWei));

            expect(winnerGte).toBe(true);
            expect(loserLt  ).toBe(true);
          }

          done();
        }));
  });
});
