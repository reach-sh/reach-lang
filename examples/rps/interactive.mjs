import * as RPS from './build/index.once.mjs';
import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import { ask, yesno, runPart
       } from '@reach-sh/stdlib/command_line_helpers.mjs';

// Set up dependencies to run locally:
// (cd ../../js && npm link)
// npm install
// npm link "@reach-sh/stdlib"

const {fromWei, toWeiBigNumber, hasRandom} = stdlib;

const name = process.argv[2];
const toUnit = fromWei;
const unit = 'ETH';

// interact methods

const getParams = async () => {
  const wagerAmount = await ask(
    `What is the wager amount in ETH? (default: 5) > `,
    (x) => toWeiBigNumber(x || '5', 'ether')
  );
  const escrowAmount = await ask(
    `What is the escrow amount in ETH? (default: 0.15) > `,
    (x) => toWeiBigNumber(x || '0.15', 'ether')
  );

  console.log(`(local: ${name} sets the parameters of the game: 
  wagerAmount: ${toUnit(wagerAmount)} ${unit}
  escrowAmount: ${toUnit(escrowAmount)} ${unit}.)`);
  return [wagerAmount, escrowAmount];
};

const acceptParams = async (wager, escrow) => {
  const response = await ask(
    `Do you accept the terms?
  wagerAmount: ${toUnit(wager)} ${unit}
  escrowAmount: ${toUnit(escrow)} ${unit}
  [y/n] (default: y) > `,
    (x) => yesno(x || 'y')
  );
  if (!response) {
    throw Error(`Terms not accepted, ${name} is outta here`);
  }

  console.log(`${name} accepts parameters of game.`);
};

const partnerIs = async (who) => {
  console.log(`${name} is playing with ${who}`);
};

const getHand = async () => {
  const res = await ask(
    `What's your hand? [ROCK, PAPER, SCISSORS] (default: ROCK) > `,
    (x) => {
      if (!x) { return 'ROCK'; }
      else if (x == 'ROCK') { return 'ROCK'; }
      else if (x == 'PAPER') { return 'PAPER'; }
      else if (x == 'SCISSORS') { return 'SCISSORS'; }
      else { throw Error(`Expected ROCK, PAPER, or SCISSORS, got ${x}`); }
    }
  );
  console.log(`(local: ${name} plays ${res}.)`);
  return res;
};

const commits = async () => {
  console.log(`${name} commits to play with (hidden) hand.`);
};

const shows = async () => {
  console.log(`${name} sends hand in clear.`);
};

const reveals = async (handB) => {
  console.log(`${name} reveals salt and hand, after learning B played ${handB}.`);
};

const endsWith = async (outcome) => {
  console.log(`${name} agrees that game is over and outcome is ${outcome}.`);
};

const interact = {
  ...hasRandom,
  getHand,
  partnerIs,
  endsWith,
  // Alice
  getParams,
  commits,
  reveals,
  // Bob
  acceptParams,
  shows,
};

runPart({
  PROG: RPS,
  allPartNames: ['A', 'B'],
  deployerPartName: 'A',
  thisPartName: name,
  greetBegin: `Let's play a game of Rock, Paper, Scissors.`,
  interact,
  stdlib,
});
