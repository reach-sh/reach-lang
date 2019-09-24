import * as RPS        from './build/rps.mjs';
import * as RPSW       from './build/rps_while.mjs';
import { runGameWith } from './index.mjs';
import { stdlibNode  } from '@reach-sh/stdlib';

const wagerInEth  = '1.5';
const escrowInEth = '0.15';

const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';

const makeInteractWith = label => (name, handf) => {
  const log = (msg) => () => { console.log(`${label} ${msg}`); return true; };
  return { params: log(`${name} publishes parameters of game: wager of ${wagerInEth}ETH and escrow of ${escrowInEth}ETH.`),
           accepts: (wagerAmount, escrowAmount) => log(`${name} accepts the terms: wager of ${wagerAmount}WEI and escrow of ${escrowAmount}WEI.`)(),
           getHand: () => { const res = handf(); log(`(local: ${name} plays ${res}.)`)(); return res; },
           commits: log(`${name} commits to play with (hidden) hand.`),
           shows: log(`${name} sends hand in clear.`),
           reveals: (handB) => log(`${name} reveals salt and hand, after learning B played ${handB}.`)(),
           outcome: log(`${name} agrees that game is over.`) }; };

const makeDemo = (doWhile, drawFirst) => {
  const label = doWhile ? `[Loop]:` : `[Single]:`;

  const introMsg =
    [ `${label} Alice initiates a new game using the \`web3\` stdlib`
    , `on the ${uri} Ethereum node.`
    ].join(' ');

  const outcomeMsgs = gs =>
    [ `${label} Alice thinks outcome is ${gs.outcomeAlice}.`
    , `${label} Bob thinks outcome is ${gs.outcomeBob}.`
    ].join('\n');

  const theRPS = doWhile ? RPSW : RPS;

  const runGameWithTheRPS = s =>
    runGameWith(theRPS
              , s
              , doWhile
              , drawFirst
              , makeInteractWith(label)
              , wagerInEth
              , escrowInEth);

  return new Promise(resolve =>
    Promise.resolve(console.log(introMsg))
      .then(() => stdlibNode(uri))
      .then(runGameWithTheRPS)
      .then(gs => console.log(outcomeMsgs(gs)))
      .then(() => console.log(`${label} Done!`))
      .then(resolve));
};

makeDemo(false, true)
  .then(() => makeDemo(false, false))
  // XXX Enable loop all of the time
  .then(() => true || makeDemo(true, true))
  .then(() => process.exit(0))
  .catch(e => console.error(e) || process.exit(1));
