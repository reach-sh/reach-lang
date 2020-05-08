import * as stdlib_eth from '@reach-sh/stdlib/ETH.mjs';
import * as stdlib_algo from '@reach-sh/stdlib/ALGO.mjs';
import * as TTT from './build/ttt.mjs';

( async () => {

  const proto = process.argv[2];
  const { stdlib, startingBalance, wagerAmount } =
        ( proto == 'ETH' ?
          { stdlib: stdlib_eth
            , startingBalance: stdlib_eth.toWeiBN('100', 'ether')
            , wagerAmount: stdlib_eth.toWeiBN('5', 'ether') }
          : ( proto == 'ALGO' ?
              { stdlib: stdlib_algo
                , startingBalance: 1000000
                , wagerAmount: 5 }
              : process.exit(1) ) );

  console.log(`\nMaking accounts\n`);

  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);

  console.log(`\nDeploying and attaching\n`);

  const ctcAlice = await alice.deploy(TTT);
  const ctcBob = await bob.attach(TTT, ctcAlice);

  console.log(`\nRunning a random game\n`);

  const interactWith = (name) => {
    return {
      getWagerAmount: () => {
        console.log(`${name} publishes parameters of game: wager of ${wagerAmount}${proto}`);
        return wagerAmount; }
      , acceptWager: (givenWagerAmount) => {
        console.log(`${name} accepts parameters of game: wager of ${givenWagerAmount}${proto}`);
        return true; }
      , getMove: (state) => {
        console.log(`${name} chooses a move from the state: ${state}`);
        const xs = state[1];
        const os = state[2];
        for ( let i = 0; i < 9; i++ ) {
          if ( ! ( xs[i] || os[i] ) ) {
            return i; } }
        throw Error(`${name} is in impossible situation: choosing a move when none is available`); }
      , showOutcome: (state) => {
        console.log(`${name} sees the final state: ${state}`);
        return true; } }; };

  const [ outcomeAlice, outcomeBob ] =
        await Promise.all([
          TTT.A(stdlib, ctcAlice, interactWith('Alice'))
          , TTT.B(stdlib, ctcBob, interactWith('Bob')) ]);

  console.log(`Alice thinks outcome is ${outcomeAlice}.`);
  console.log(`Bob thinks outcome is ${outcomeBob}.`);

  console.log(`Done!`);
  process.exit(0); })();
