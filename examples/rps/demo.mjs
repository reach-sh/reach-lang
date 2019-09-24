import * as RPS        from './build/rps.mjs';
import * as RPSW       from './build/rps_while.mjs';
import { randomHand, runGameWith } from './index.mjs';

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

const makeDemo = async (theRPS, getHand) => {
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

  const gs = await
  runGameWith(theRPS
              , interactWith('Alice')
              , interactWith('Bob')
              , wagerInEth
              , escrowInEth);
  console.log(`Alice thinks outcome is ${gs.outcomeAlice}.`);
  console.log(`Bob thinks outcome is ${gs.outcomeBob}.`);
  console.log(`Done!`);

  return; };

( async () => {
  console.log(`\nRunning game that will Draw\n`);
  await makeDemo(RPS, staticHand('ROCK'));

  console.log(`\nRunning game that may Draw\n`);
  await makeDemo(RPS, randomHand);

  if ( process.env.RPS_WHILE ) {
    console.log(`\nRunning game that may not Draw\n`);
    await makeDemo(RPSW, onceThen(staticHand('PAPER'), randomHand)); }

  console.log(`\nAll games are complete!\n`);
  process.exit(0);
})();
