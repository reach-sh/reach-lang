import * as stdlib from '@reach-sh/stdlib/ALGO.mjs';
import * as RPS from './build/rps.mjs';

( async () => {

  const wager = 15;
  const escrow = 25;
  const startingBalance = 1 * 1000000;

  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);
  const obs = await stdlib.newTestAccount(startingBalance);

  const demo = async (theRPS, getHand) => {
    console.log(`Alice initiates a new game.`);

    const interactWith = (name) => {
      const log = (msg) => () => { console.log(`${msg}`); return true; };
      return { params: log(`${name} publishes parameters of game: wager of ${wager}ALGO and escrow of ${escrow}ALGO.`),
               accepts: (wagerAmount, escrowAmount) => log(`${name} accepts the terms: wager of ${wagerAmount}ALGO and escrow of ${escrowAmount}ALGO.`)(),
               getHand: async () => { const res = await getHand(); log(`(local: ${name} plays ${res}.)`)(); return res; },
               commits: log(`${name} commits to play with (hidden) hand.`),
               shows: log(`${name} sends hand in clear.`),
               reveals: (handB) => log(`${name} reveals salt and hand, after learning B played ${handB}.`)(),
               whilecount: (count) => log(`${name} observed ${count} rounds of the loop`)(),
               outcome: log(`${name} agrees that game is over.`) }; };

    const ctcAlice = await alice.deploy(theRPS);
    const ctcBob = await bob.attach(theRPS, ctcAlice.address,
                                    ctcAlice.creation_block);
    const ctcObs = await obs.attach(theRPS, ctcAlice.address,
                                    ctcAlice.creation_block);

    const [ outcomeAlice, outcomeBob, outcomeObs ] =
          await Promise.all([
            theRPS.A(stdlib, ctcAlice, interactWith('Alice'),
                     wager, escrow),
            theRPS.B(stdlib, ctcBob, interactWith('Bob')),
            theRPS.O(stdlib, ctcObs, interactWith('Observer'))]);

    console.log(`Alice thinks outcome is ${outcomeAlice}.`);
    console.log(`Bob thinks outcome is ${outcomeBob}.`);
    console.log(`Observer thinks outcome is ${outcomeObs}.`);
    console.log(`Done!`); };

  console.log(`\nRunning game that will Draw\n`);
  const staticHand = (hand) => () => hand;
  await demo(RPS, staticHand('ROCK'));

  console.log(`\nRunning game that may Draw\n`);
  const randomArray = a => a[ Math.floor(Math.random() * a.length) ];
  const randomHand = () => randomArray([ 'ROCK', 'PAPER', 'SCISSORS' ]);
  await demo(RPS, randomHand);

  console.log(`\nRunning game that times out\n`);
  const delayHand = async () => {
    for ( let i = 0; i < 10; i++ ) {
      console.log(`\tAlice takes her sweet time...`);
      await stdlib.transfer(alice.address, alice.address, wager); }
    return randomHand(); };
  await demo(RPS, delayHand);

  console.log(`\nAll games are complete!\n`);
  process.exit(0); })();
