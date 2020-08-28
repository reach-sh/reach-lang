import * as stdlib_eth from '@reach-sh/stdlib/ETH.mjs';
import * as stdlib_algo from '@reach-sh/stdlib/ALGO.mjs';
import * as stdlib_fake from '@reach-sh/stdlib/FAKE.mjs';
import * as NIM from './build/index.main.mjs';

( async () => {

  const proto = process.argv[2];
  const { stdlib, startingBalance, wagerAmount } =
        ( proto == 'ETH' ?
          { stdlib: stdlib_eth
            , startingBalance: stdlib_eth.toWeiBigNumber('100', 'ether')
            , wagerAmount: stdlib_eth.toWeiBigNumber('5', 'ether') }
          : ( proto == 'ALGO' ?
              { stdlib: stdlib_algo
                , startingBalance: 1000000
                , wagerAmount: 5 }
              : ( proto == 'FAKE' ) ?
              { stdlib: stdlib_fake
                , startingBalance: 1000000
                , wagerAmount: 5 }
              : process.exit(1) ) );

  console.log(`\nMaking accounts\n`);

  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);

  console.log(`\nDeploying and attaching\n`);

  const ctcAlice = await alice.deploy(NIM);
  const ctcBob = await bob.attach(NIM, ctcAlice);

  console.log(`\nRunning a random game\n`);

  const interactWith = (name) => {
    return {
      ...stdlib.hasRandom,
      getParams: () => {
        console.log(`${name} publishes parameters of game: wager of ${wagerAmount}${proto} and heap is 21`);
        return [ wagerAmount, stdlib.bigNumberify(21) ]; }
      , acceptParams: (givenWagerAmount, givenInitialHeap) => {
        console.log(`${name} accepts parameters of game: wager of ${givenWagerAmount}${proto} and heap of ${givenInitialHeap}`); }
      , getMove: (heap1, heap2) => {
        console.log(`${name} chooses a heap from: ${heap1} and ${heap2} with amount 1`);
        return [ stdlib.gt(heap1, heap2), stdlib.bigNumberify(1) ]; }
      , showOutcome: (outcome) => {
        console.log(`${name} sees the final outcome: ${outcome}`); } }; };

  const done =
        await Promise.all([
          NIM.A(stdlib, ctcAlice, interactWith('Alice'))
          , NIM.B(stdlib, ctcBob, interactWith('Bob')) ]);
  void(done);

  console.log(`Done!`);
  process.exit(0); })();
