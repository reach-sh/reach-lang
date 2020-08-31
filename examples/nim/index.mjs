import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as NIM from './build/index.main.mjs';

( async () => {
  const connector = stdlib_loader.getConnector();
  const stdlib = await stdlib_loader.loadStdlib();
  const { startingBalance, wagerAmount } =
        ( connector == 'ETH' ? {
          startingBalance: stdlib.toWeiBigNumber('100', 'ether'),
          wagerAmount: stdlib.toWeiBigNumber('5', 'ether'),
        } : ( connector == 'ALGO' ? {
          stdlib: stdlib,
          startingBalance: 1000000,
          wagerAmount: 5,
        } : ( connector == 'FAKE' ) ? {
          startingBalance: 1000000,
          wagerAmount: 5,
        } : process.exit(1) ) );

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
        console.log(`${name} publishes parameters of game: wager of ${wagerAmount}${connector} and heap is 21`);
        return [ wagerAmount, stdlib.bigNumberify(21) ]; }
      , acceptParams: (givenWagerAmount, givenInitialHeap) => {
        console.log(`${name} accepts parameters of game: wager of ${givenWagerAmount}${connector} and heap of ${givenInitialHeap}`); }
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
