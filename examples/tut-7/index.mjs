import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import { ask, yesno, done } from '@reach-sh/stdlib/ask.mjs';
import * as backend from './build/index.main.mjs';

( async () => {
  const toNetworkFormat = (n) => stdlib.toWeiBigNumber(n, 'ether');

  const isAlice = await ask(
    `Are you Alice?`, yesno);
  const who = isAlice ? 'Alice' : 'Bob';

  console.log(`Starting Rock, Paper, Scissors as ${who}`);

  let acc = null;
  if ( await ask(
    `Would you like to create an account? (only possible on devnet)`, yesno) ) {
    acc = await stdlib.newTestAccount( toNetworkFormat('1000') ); }
  else {
    const phrase = await ask(
      `What is your account mnemonic?`, (x => x));
    acc = await stdlib.newAccountFromMnemonic(phrase); }

  const doDeploy = await ask(
    `Do you want to deploy the game? (y/n)`, yesno);

  let ctc = null;
  if ( doDeploy ) {
    ctc = await acc.deploy(backend);
    console.log(`The contract is deployed as = ${ctc.info}`); }
  else {
    const info = await ask(
      `Please paste the contract information:`,
      stdlib.ctcFromInfo );
    ctc = await acc.attach(backend, info); }

  const getBalance = async () =>
        stdlib.fromWei ( await stdlib.balanceOf(acc) );

  const before = await getBalance();
  console.log(`Your balance is ${before}`);

  const interact = { ...stdlib.hasRandom };

  interact.informTimeout = () => {
    console.log(`There was a timeout.`);
    process.exit(1); };

  if ( isAlice ) {
    const amt = await ask(
      `How much do you want to wager?`,
      toNetworkFormat );
    interact.wager = amt; }
  else {
    interact.acceptWager =
      async (amt) => {
        if ( await ask(
          `Do you accept the wager of ${stdlib.fromWei(amt)}?`,
          yesno) ) {
          return; }
        else {
          process.exit(0); } } }

  const HAND = ['Rock', 'Paper', 'Scissors'];
  const HANDS = {'Rock': 0, 'R': 0, 'r': 0,
                 'Paper': 1, 'P': 1, 'p': 1,
                 'Scissors': 2, 'S': 2, 's': 2 };
  interact.getHand =
    async () => {
      const hand = await ask(
        `What hand will you play?`,
        (x) => {
          const hand = HANDS[x];
          if ( hand == null ) {
            throw Error(`Not a valid hand ${hand}`); }
          return hand; } );
      console.log(`You played ${HAND[hand]}`);
      return hand; };

  const OUTCOME = ['Bob wins', 'Draw', 'Alice wins'];
  interact.seeOutcome =
    async (outcome) => {
      console.log(`The outcome is: ${OUTCOME[outcome]}`); };

  const part = isAlice ? backend.Alice : backend.Bob;
  await part(stdlib, ctc, interact);

  const after = await getBalance();
  console.log(`Your balance is now ${after}`);

  done();
})();
