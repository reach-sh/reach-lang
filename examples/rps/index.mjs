import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as RPS from './build/index.main.mjs';

(async () => {
  const stdlib = await stdlib_loader.loadStdlib();

  const startingBalance = stdlib.parseCurrency(100);
  const escrowAmount = stdlib.parseCurrency(1);
  const wagerAmount = stdlib.parseCurrency(5);

  const dispAmt = (x) => `${stdlib.formatCurrency(x)} ${stdlib.standardUnit}`;

  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);
  const obs = await stdlib.newTestAccount(startingBalance);

  const demo = async (theRPS, getHand) => {
    console.log(`Alice initiates a new game.`);

    const log = (msg, ret = null) => () => { console.log(`${msg}`); return ret; };
    const interactWith = (name) => ({
      ...stdlib.hasRandom,
      getParams: log(`(local: ${name} returns wagerAmount ${dispAmt(wagerAmount)} and escrowAmount ${dispAmt(escrowAmount)}.)`, [wagerAmount, escrowAmount]),
      params: log(`${name} publishes parameters of game: wager of ${dispAmt(wagerAmount)} and escrow of ${dispAmt(escrowAmount)}.`),
      acceptParams: (wager, escrow) => log(`${name} accepts the terms: wager of ${dispAmt(wager)} and escrow of ${dispAmt(escrow)}.`)(),
      partnerIs: (who) => log(`${name} is playing with ${who}`)(),
      getHand: async () => { const res = await getHand(); log(`(local: ${name} plays ${res}.)`)(); return res; },
      commits: log(`${name} commits to play with (hidden) hand.`),
      shows: log(`${name} sends hand in clear.`),
      reveals: (handB) => log(`${name} reveals salt and hand, after learning B played ${handB}.`)(),
      endsWith: (outcome) => log(`${name} agrees that game is over and outcome is ${outcome}.`)()
    });

    const ctcAlice = await alice.deploy(theRPS);
    const ctcBob = await bob.attach(theRPS, ctcAlice.getInfo());
    const ctcObs = await obs.attach(theRPS, ctcAlice.getInfo());

    await Promise.all([
      theRPS.A(stdlib, ctcAlice, interactWith('Alice')),
      theRPS.B(stdlib, ctcBob, interactWith('Bob')),
      theRPS.O(stdlib, ctcObs, interactWith('Observer')),
    ]);

    console.log(`Done!`);
  };

  console.log(`\nRunning game that may Draw\n`);
  const randomArray = a => a[ Math.floor(Math.random() * a.length) ];
  const randomHand = () => randomArray([ 'ROCK', 'PAPER', 'SCISSORS' ]);
  await demo(RPS, randomHand);

  console.log(`\nRunning game that times out\n`);
  const delayHand = async () => {
    for ( let i = 0; i < 10; i++ ) {
      console.log(`\tAlice takes her sweet time...`);
      await stdlib.wait(1);
    }
    return randomHand();
  };
  await demo(RPS, delayHand);

  console.log(`\nRunning game that may not Draw\n`);
  const staticHand = (hand) => () => hand;
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
  await demo(RPS, onceThen(staticHand('PAPER'), randomHand));

  console.log(`\nAll games are complete!\n`);
  process.exit(0);
})();
