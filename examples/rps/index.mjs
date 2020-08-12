import * as stdlib_eth from '@reach-sh/stdlib/ETH.mjs';
import * as stdlib_algo from '@reach-sh/stdlib/ALGO.mjs';
import * as RPS from './build/index.once.mjs';
import * as RPSW from './build/index.nodraw.mjs';

( async () => {
  const proto = process.argv[2];
  const { stdlib, startingBalance, escrowAmount, wagerAmount, toUnit, unit } = (
    proto == 'ETH' ? {
      stdlib: stdlib_eth,
      startingBalance: stdlib_eth.toWeiBN('100', 'ether'),
      escrowAmount: stdlib_eth.toWeiBN('0.15', 'ether'),
      wagerAmount: stdlib_eth.toWeiBN('1.5', 'ether'),
      toUnit: stdlib_eth.fromWei,
      unit: 'ether',
    } : proto == 'ALGO' ? {
      stdlib: stdlib_algo,
      startingBalance: 1 * 1000000,
      escrowAmount: 25,
      wagerAmount: 15,
      toUnit: (x) => x,
      unit: 'microAlgo',
    } : process.exit(1)
  );

  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);
  const obs = await stdlib.newTestAccount(startingBalance);

  const demo = async (theRPS, getHand) => {
    console.log(`Alice initiates a new game.`);

    const interactWith = (name) => {
      const log = (msg, ret = null) => () => { console.log(`${msg}`); return ret; };
      return { ...stdlib.hasRandom,
               getParams: log(`(local: ${name} returns wagerAmount ${toUnit(wagerAmount)} ${unit} and escrowAmount ${toUnit(escrowAmount)} ${unit}.)`, [wagerAmount, escrowAmount]),
               params: log(`${name} publishes parameters of game: wager of ${toUnit(wagerAmount)} ${unit} and escrow of ${toUnit(escrowAmount)} ${unit}.`),
               acceptParams: (wager, escrow) => log(`${name} accepts the terms: wager of ${toUnit(wager)} ${unit} and escrow of ${toUnit(escrow)} ${unit}.`)(),
               partnerIs: (who) => log(`${name} is playing with ${who}`)(),
               getHand: async () => { const res = await getHand(); log(`(local: ${name} plays ${res}.)`)(); return res; },
               commits: log(`${name} commits to play with (hidden) hand.`),
               shows: log(`${name} sends hand in clear.`),
               reveals: (handB) => log(`${name} reveals salt and hand, after learning B played ${handB}.`)(),
               whilecount: (count) => log(`${name} observed ${count} rounds of the loop`)(),
               outcome: log(`${name} agrees that game is over.`) }; };

    const ctcAlice = await alice.deploy(theRPS);
    const ctcBob = await bob.attach(theRPS, ctcAlice);
    const ctcObs = await obs.attach(theRPS, ctcAlice);

    const [ outcomeAlice, outcomeBob, outcomeObs ] =
          await Promise.all([
            theRPS.A(stdlib, ctcAlice, interactWith('Alice')),
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
      await stdlib.transfer(alice.networkAccount, alice.networkAccount, wagerAmount); }
    return randomHand(); };
  await demo(RPS, delayHand);

  console.log(`\nRunning game that may not Draw\n`);
  const onceThen = (first, after) => {
    let called = 0;
    return () => {
      if (called == 2) {
        return after(); }
      else {
        called++;
        return first(); } }; };
  await demo(RPSW, onceThen(staticHand('PAPER'), randomHand));

  console.log(`\nAll games are complete!\n`);
  process.exit(0); })();
