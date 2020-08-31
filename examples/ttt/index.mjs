import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
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

  const connector = stdlib_loader.getConnector();
  const stdlib = await stdlib_loader.loadStdlib();
  const { startingBalance, wagerAmount } =
        ( connector == 'ETH' ?
          { stdlib: stdlib
            , startingBalance: stdlib.toWeiBigNumber('100', 'ether')
            , wagerAmount: stdlib.toWeiBigNumber('5', 'ether') }
          : ( connector == 'ALGO' ?
              { stdlib: stdlib
                , startingBalance: 1000000
                , wagerAmount: 5 }
              : (() => {
                console.log(`Unknown connector: ${connector}`);
                process.exit(1); })() ) );

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
        console.log(`${name} publishes parameters of game: wager of ${wagerAmount}${connector}`);
        return wagerAmount; }
      , acceptWager: (givenWagerAmount) => {
        console.log(`${name} accepts parameters of game: wager of ${givenWagerAmount}${connector}`); }
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
