import * as RPS        from './build/rps.mjs';
import * as RPSW       from './build/rps_while.mjs';
import { runGameWith } from './index.mjs';

const wagerInEth  = '1.5';
const escrowInEth = '0.15';

const makeDemo = async (doWhile, drawFirst) => {
  const label = doWhile ? `[Loop]:` : `[Single]:`;

  console.log(`${label} Alice initiates a new game.`);

  const interactWith = (name, handf) => {
    const log = (msg) => () => { console.log(`${label} ${msg}`); return true; };
    return { params: log(`${name} publishes parameters of game: wager of ${wagerInEth}ETH and escrow of ${escrowInEth}ETH.`),
             accepts: (wagerAmount, escrowAmount) => log(`${name} accepts the terms: wager of ${wagerAmount}WEI and escrow of ${escrowAmount}WEI.`)(),
             getHand: () => { const res = handf(); log(`(local: ${name} plays ${res}.)`)(); return res; },
             commits: log(`${name} commits to play with (hidden) hand.`),
             shows: log(`${name} sends hand in clear.`),
             reveals: (handB) => log(`${name} reveals salt and hand, after learning B played ${handB}.`)(),
             outcome: log(`${name} agrees that game is over.`) }; };

  const theRPS = doWhile ? RPSW : RPS;
  const gs = await runGameWith(theRPS
                               , drawFirst
                               , interactWith
                               , wagerInEth
                               , escrowInEth);
  console.log(`${label} Alice thinks outcome is ${gs.outcomeAlice}.`);
  console.log(`${label} Bob thinks outcome is ${gs.outcomeBob}.`);
  console.log(`${label} Done!`);

  return; };

( async () => {
  await makeDemo(false, true);
  await makeDemo(false, false);
  if ( process.env.RPS_WHILE ) {
    // XXX Enable loop all of the time
    await makeDemo(true, true); }
  process.exit(0);
})();
