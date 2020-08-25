import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as backend from './build/index.main.mjs';

( async () => {
  const toNetworkFormat = (n) => stdlib.toWeiBigNumber(n, 'ether');

  const accAlice = await stdlib.newTestAccount(toNetworkFormat('10'));
  const accBob = await stdlib.newTestAccount(toNetworkFormat('10'));

  const ctcAlice = await accAlice.deploy(backend);
  const ctcBob = await accBob.attach(backend, ctcAlice);

  const HAND = ['Rock', 'Paper', 'Scissors'];
  const OUTCOME = ['Bob wins', 'Draw', 'Alice wins'];
  const Player = (Who) => ({
    getHand: () => { const hand = Math.floor(Math.random()*3);
                     console.log(`${Who} played ${HAND[hand]}`);
                     return hand; },
    seeOutcome: (outcome) =>
    console.log(`${Who} saw outcome ${OUTCOME[outcome]}`) });
  
  await Promise.all([
    backend.Alice(
      stdlib, ctcAlice,
      Player('Alice')),
    backend.Bob(
      stdlib, ctcBob,
      Player('Bob'))
  ]);
})();
