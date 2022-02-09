// a modified copy of rps-3
import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

let reqCount = 0;
const mmbr = 500;
const mmbrTestEnabled = stdlib.connector === 'ALGO';

if (mmbrTestEnabled) {
  console.log('mmbr test enabled');
  stdlib.setMinMillisBetweenRequests(mmbr);
  stdlib.setCustomHttpEventHandler((e) => {
    const now = new Date();
    console.log(now, [
      e.label,
      e.reqNum,
      e.method,
      e.eventName,
      // e.relativePath,
    ]);
    if (e.eventName === 'before') reqCount++;
  });
}

  const startMs = Date.now();
  const startingBalance = stdlib.parseCurrency(100);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const getBalance = async (who) => fmt(await stdlib.balanceOf(who));
  const beforeAlice = await getBalance(accAlice);
  const beforeBob = await getBalance(accBob);

  const ctcAlice = accAlice.contract(backend);
  const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

  const HAND = ['Rock', 'Paper', 'Scissors'];
  const OUTCOME = ['Bob wins', 'Draw', 'Alice wins'];
  const Player = (Who) => ({
    getHand: () => {
      const hand = Math.floor(Math.random() * 3);
      console.log(`${Who} played ${HAND[hand]}`);
      return hand;
    },
    seeOutcome: (outcome) => {
      console.log(`${Who} saw outcome ${OUTCOME[outcome]}`);
    },
  });

  await Promise.all([
    ctcAlice.p.Alice({
      ...Player('Alice'),
      wager: stdlib.parseCurrency(5),
    }),
    ctcBob.p.Bob({
      ...Player('Bob'),
      acceptWager: (amt) => {
        console.log(`Bob accepts the wager of ${fmt(amt)}.`);
      },
    }),
  ]);

  const afterAlice = await getBalance(accAlice);
  const afterBob = await getBalance(accBob);

  console.log(`Alice went from ${beforeAlice} to ${afterAlice}.`);
  console.log(`Bob went from ${beforeBob} to ${afterBob}.`);

  const elapsedMs = Date.now() - startMs;
  const expectedMinElapsedMs = (reqCount - 1) * mmbr;

  if (mmbrTestEnabled) {
    console.log(`A total of ${reqCount} http requests occured`);
    console.log(`This was expected to take over ${expectedMinElapsedMs}ms due to intentional client-side rate limiting.`);
    console.log(`It actually took ${elapsedMs}ms.`);
    if (elapsedMs < expectedMinElapsedMs) {
      throw Error(`The total runtime was too short.`);
    }
  }
