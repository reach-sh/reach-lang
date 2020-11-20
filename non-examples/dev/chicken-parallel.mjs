import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/chicken-parallel.main.mjs';

const generateRandom = (min, max) =>
  Math.floor(Math.random() * (max - min + 1) + min)

const main = async () => {
  const stdlib = await loadStdlib();
  const { formatCurrency, parseCurrency, newTestAccount } = stdlib;

  const fmt = amt => formatCurrency(amt, 4);

  const getBalance = async (acc) =>
    fmt(await stdlib.balanceOf(acc));

  const startingBalance = parseCurrency(100);

  const accAlice = await newTestAccount(startingBalance);
  const accBob   = await newTestAccount(startingBalance);

  // Track balances of Alice & Bob
  const balances = {
    'Alice': { before: await getBalance(accAlice) },
    'Bob'  : { before: await getBalance(accBob) },
  };

  const ctcAlice = accAlice.deploy(backend);
  const ctcBob   = accBob.attach(backend, ctcAlice.getInfo());

  // Randomly choose when Alice and Bob should stop.
  const counters = {
    'Alice': generateRandom(1, 10),
    'Bob'  : generateRandom(1, 10),
  };

  const interactWith = (name) => ({
    showOutcome: (outcome) => {
      console.log(`${name} sees the final outcome: ${outcome}`);},
    keepGoing: () => {
      if (counters[name] === 0) {
        console.log(`${name} stops going.`);
        return false;
      } else {
        console.log(`${name} keeps going.`);
        counters[name] -= 1;
        return true; }
    },
  });

  await Promise.all([
    backend.Alice(stdlib, ctcAlice, {
      ...interactWith('Alice'),
      getParams: () => ({
        wager: stdlib.parseCurrency(5),
        deadline: 11 }),
    }),
    backend.Bob(stdlib, ctcBob, {
      ...interactWith('Bob'),
      confirmWager: (amount) => {
        console.log(`Bob accepts the wager of ${fmt(amount)}.`); },
    }),
  ]);

  balances['Alice'].after = await getBalance(accAlice);
  balances['Bob'].after   = await getBalance(accBob);

  for (const [acc, bals] of Object.entries(balances)) {
    console.log(`${acc} went from ${bals.before} to ${bals.after}`); }

};

main()
