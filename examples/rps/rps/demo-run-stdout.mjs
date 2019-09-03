// vim: filetype=javascript

import * as RPS        from '../build/rps.mjs';
import * as RPSW       from '../build/rps_while.mjs';
import { runGameWith } from './demo.mjs';
import { stdlibNode  } from './stdlib/web3/node.mjs';

const wagerInEth  = '1.5';
const escrowInEth = '0.15';

const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';

const makeInteractWith = label => (name, handf) => (a, cb) => {
  const res = a === 'getHand' ? handf() : '';

  const paramsMsg =
    [ `${name} publishes parameters of game:`
    , `wager of ${wagerInEth}ETH`
    , `and escrow of ${escrowInEth}ETH.`
    ].join(' ');

  const msg
    = a === 'params'  ? paramsMsg
    : a === 'accepts' ? `${name} accepts the terms.`
    : a === 'getHand' ? `(local: ${name} plays ${res}.)`
    : a === 'commits' ? `${name} commits to play with (hidden) hand.`
    : a === 'shows'   ? `${name} sends hand in clear.`
    : a === 'reveals' ? `${name} reveals salt and hand.`
    : a === 'outcome' ? `${name} agrees that game is over.`
    : null;

  !!msg && console.log(`${label} ${msg}`);

  return cb(res);
};

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
              , escrowInEth
              , uri);

  return new Promise(resolve =>
    Promise.resolve(console.log(introMsg))
      .then(() => stdlibNode(theRPS.ABI, theRPS.Bytecode, uri))
      .then(runGameWithTheRPS)
      .then(gs => console.log(outcomeMsgs(gs)))
      .then(() => console.log(`${label} Done!`))
      .then(resolve));
};

makeDemo(true, true)
  .then(() => makeDemo(false, true))
  .then(() => makeDemo(false, false))
  .then(() => process.exit(0))
  .catch(e => console.error(e) || process.exit(1));
