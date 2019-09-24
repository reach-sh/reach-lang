import * as RPS        from './build/rps.mjs';
import { runTests, assert } from '@reach-sh/stdlib/tester.mjs';
import { connect } from '@reach-sh/stdlib';
const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';

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

  const stdlib = connect(uri);

  const wagerInWei = stdlib.toBN(stdlib.toWei(wagerInEth, 'ether'));
  const escrowInWei = stdlib.toBN(stdlib.toWei(escrowInEth, 'ether'));

  const { balanceOf } = stdlib;

  const startingBalance = stdlib.toBN(stdlib.toWei('100', 'ether'));

  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);
  const balanceStartAlice = await balanceOf(alice);
  const balanceStartBob = await balanceOf(bob);
  const ctors = [ alice.userAddress, bob.userAddress ];

  const ctcAlice =
        await alice.deploy(RPS.ABI, RPS.Bytecode, ctors);
  const ctcBob =
        await bob.attach(RPS.ABI, ctors, ctcAlice.address,
                         ctcAlice.creation_block);

  const [ outcomeBob, outcomeAlice ] =
        await Promise.all([
          RPS.B(ctcBob, interactWithBob),
          RPS.A(ctcAlice, interactWithAlice,
                wagerInWei, escrowInWei)]);

  const balanceEndAlice = await balanceOf(alice);
  const balanceEndBob = await balanceOf(bob);

  assert.deepEqual(outcomeAlice, outcomeBob, `outcomes disagree`);

  assert.deepEqual(outcomeAlice, ['Alice wins']);

  const [ balStartWinner, balStartLoser, balEndWinner, balEndLoser ] =
        [ balanceStartAlice, balanceStartBob, balanceEndAlice, balanceEndBob ];

  assert.ok(balEndWinner.gte(balStartWinner), `winner' >= winner`);
  assert.ok(balEndLoser.lt(balStartLoser), `loser' < loser`);
});
