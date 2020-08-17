import * as stdlib_eth from '@reach-sh/stdlib/ETH.mjs';
import * as stdlib_algo from '@reach-sh/stdlib/ALGO.mjs';
import * as TTT from './build/index.main.mjs';

function render(st) {
  let o = '\n\t';
  for ( let i = 0; i < 9; i++ ) {
    o += st.xs[i] ? 'X' : st.os[i] ? 'O' : ' ';
    if ( i != 8 ) {
      o += (i % 3 == 2) ? '\n\t-----\n\t' : '|'; } }
  o += '\n';
  return o; }

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
      ...stdlib.hasRandom
      , getWager: () => {
        console.log(`${name} publishes parameters of game: wager of ${wagerAmount}${proto}`);
        return wagerAmount; }
      , acceptWager: (givenWagerAmount) => {
        console.log(`${name} accepts parameters of game: wager of ${givenWagerAmount}${proto}`); }
      , getMove: (state) => {
        console.log(`${name} chooses a move from the state:${render(state)}`);
        const xs = state.xs;
        const os = state.os;
        while ( xs && os ) {
          const i = Math.floor( Math.random() * 9 );
          if ( ! ( xs[i] || os[i] ) ) {
            return i; } }  }
      , endsWith: (state) => {
        console.log(`${name} sees the final state: ${render(state)}`); } }; };

  await Promise.all([
    TTT.A(stdlib, ctcAlice, interactWith('Alice')),
    TTT.B(stdlib, ctcBob, interactWith('Bob')) ]);

  console.log(`Done!`);
  process.exit(0); })();
