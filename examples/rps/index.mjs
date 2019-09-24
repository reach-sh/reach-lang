import { connect } from '@reach-sh/stdlib';
import * as RPS        from './build/rps.mjs';
import * as RPSW       from './build/rps_while.mjs';

const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';

const randomArray = a => a[ Math.floor(Math.random() * a.length) ];
export const randomHand = () => randomArray([ 'ROCK', 'PAPER', 'SCISSORS' ]);

const onceThen = (first, after) => {
  let called = 0;
  return () => {
    if (called == 2) {
      return after();
    } else {
      called++;
      return first();
    }
  };
};

const staticHand = (hand) => () => hand;

const wagerInEth  = '1.5';
const escrowInEth = '0.15';

const demo = async (theRPS, getHand) => {
  console.log(`Alice initiates a new game.`);

  const interactWith = (name) => {
    const log = (msg) => () => { console.log(`${msg}`); return true; };
    return { params: log(`${name} publishes parameters of game: wager of ${wagerInEth}ETH and escrow of ${escrowInEth}ETH.`),
             accepts: (wagerAmount, escrowAmount) => log(`${name} accepts the terms: wager of ${wagerAmount}WEI and escrow of ${escrowAmount}WEI.`)(),
             getHand: () => { const res = getHand(); log(`(local: ${name} plays ${res}.)`)(); return res; },
             commits: log(`${name} commits to play with (hidden) hand.`),
             shows: log(`${name} sends hand in clear.`),
             reveals: (handB) => log(`${name} reveals salt and hand, after learning B played ${handB}.`)(),
             outcome: log(`${name} agrees that game is over.`) }; };

  const stdlib = connect(uri);
  const wagerInWei = stdlib.toBN(stdlib.toWei(wagerInEth,  'ether'));
  const escrowInWei = stdlib.toBN(stdlib.toWei(escrowInEth, 'ether'));

  const startingBalance = stdlib.toBN(stdlib.toWei('100', 'ether'));
  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);

  const ctors = [ alice.userAddress, bob.userAddress ];

  const ctcAlice =
        await alice.deploy(theRPS.ABI, theRPS.Bytecode, ctors);
  const ctcBob =
        await bob.attach(theRPS.ABI, ctors, ctcAlice.address,
                         ctcAlice.creation_block);

  const [ outcomeBob, outcomeAlice ] =
        await Promise.all([
          theRPS.B(ctcBob, interactWith('Bob')),
          theRPS.A(ctcAlice, interactWith('Alice'),
                   wagerInWei, escrowInWei)]);

  console.log(`Alice thinks outcome is ${outcomeAlice}.`);
  console.log(`Bob thinks outcome is ${outcomeBob}.`);
  console.log(`Done!`);

  return; };

( async () => {
  console.log(`\nRunning game that will Draw\n`);
  await demo(RPS, staticHand('ROCK'));

  console.log(`\nRunning game that may Draw\n`);
  await demo(RPS, randomHand);

  if ( process.env.RPS_WHILE ) {
    console.log(`\nRunning game that may not Draw\n`);
    await demo(RPSW, onceThen(staticHand('PAPER'), randomHand)); }

  console.log(`\nAll games are complete!\n`);
  process.exit(0);
})();
