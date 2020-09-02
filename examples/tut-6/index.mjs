import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as backend from './build/index.main.mjs';

( async () => {
  const toNetworkFormat = (n) => stdlib.toWeiBigNumber(n, 'ether');
  const accAlice = await stdlib.newTestAccount(toNetworkFormat('10'));
  const accBob = await stdlib.newTestAccount(toNetworkFormat('10'));

  const getBalance = async (who) => stdlib.fromWei ( await stdlib.balanceOf(who) );
  const beforeAlice = await getBalance(accAlice);
  const beforeBob = await getBalance(accBob);

  const ctcAlice = await accAlice.deploy(backend);
  const ctcBob = await accBob.attach(backend, ctcAlice);

  const HAND = ['Rock', 'Paper', 'Scissors'];
  const OUTCOME = ['Bob wins', 'Draw', 'Alice wins'];
  const Player = (Who) => ({
    ...stdlib.hasRandom,
    getHand: async () => { // <-- async now
      const hand = Math.floor(Math.random()*3);
      console.log(`${Who} played ${HAND[hand]}`);
      if ( Math.random() <= 0.01 ) {
        for ( let i = 0; i < 10; i++ ) {
          console.log(`  ${Who} takes their sweet time sending it back...`);
          await stdlib.wait(1); } }
      return hand; },
    seeOutcome: (outcome) =>
    console.log(`${Who} saw outcome ${OUTCOME[outcome]}`),
    informTimeout: () =>
    console.log(`${Who} observed a timeout`) });

  await Promise.all([
    backend.Alice(
      stdlib, ctcAlice,
      { ...Player('Alice'),
        wager: toNetworkFormat('5')
      }),
    backend.Bob(
      stdlib, ctcBob,
      { ...Player('Bob'),
        acceptWager: (amt) =>
        console.log(`Bob accepts the wager of ${stdlib.fromWei(amt)}.`)  } )
  ]);

  const afterAlice = await getBalance(accAlice);
  const afterBob = await getBalance(accBob);

  console.log(`Alice went from ${beforeAlice} to ${afterAlice}.`);
  console.log(`Bob went from ${beforeBob} to ${afterBob}.`);

})();
