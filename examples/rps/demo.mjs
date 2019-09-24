import * as RPS        from './build/rps.mjs';
import * as RPSW       from './build/rps_while.mjs';
import { randomHand, runGameWith } from './index.mjs';

export const makeDrawFirstHand = first => {
  let called = false;
  return () => {
    if (called) {
      return randomHand();
    } else {
      called = true;
      return first;
    }
  };
};

const wagerInEth  = '1.5';
const escrowInEth = '0.15';

const makeDemo = async (theRPS, drawFirst) => {
  console.log(`Alice initiates a new game.`);

  const interactWith = (name, handf) => {
    const log = (msg) => () => { console.log(`${msg}`); return true; };
    return { params: log(`${name} publishes parameters of game: wager of ${wagerInEth}ETH and escrow of ${escrowInEth}ETH.`),
             accepts: (wagerAmount, escrowAmount) => log(`${name} accepts the terms: wager of ${wagerAmount}WEI and escrow of ${escrowAmount}WEI.`)(),
             getHand: () => { const res = handf(); log(`(local: ${name} plays ${res}.)`)(); return res; },
             commits: log(`${name} commits to play with (hidden) hand.`),
             shows: log(`${name} sends hand in clear.`),
             reveals: (handB) => log(`${name} reveals salt and hand, after learning B played ${handB}.`)(),
             outcome: log(`${name} agrees that game is over.`) }; };

  const shared = randomHand();

  const makeWhichHand = drawFirst
        ? () => makeDrawFirstHand(shared)
        : () => randomHand;

  const gs = await
  runGameWith(theRPS
              , interactWith('Alice', makeWhichHand())
              , interactWith('Bob', makeWhichHand())
              , wagerInEth
              , escrowInEth);
  console.log(`Alice thinks outcome is ${gs.outcomeAlice}.`);
  console.log(`Bob thinks outcome is ${gs.outcomeBob}.`);
  console.log(`Done!`);

  return; };

( async () => {
  console.log(`\nRunning games that may Draw\n`);
  await makeDemo(RPS, true);
  await makeDemo(RPS, false);

  // XXX Enable loop all of the time
  if ( process.env.RPS_WHILE ) {
    console.log(`\nRunning games that may not Draw\n`);
    await makeDemo(RPSW, true); }

  console.log(`\nAll games are complete!\n`);
  process.exit(0);
})();
