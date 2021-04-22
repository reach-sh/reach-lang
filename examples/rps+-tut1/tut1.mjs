import { loadStdlib } from '@reach-sh/stdlib';
//import * as stdlib from '@reach-sh/stdlib/ALGO.mjs'
import * as backend from './build/tut1.main.mjs';
import { ask, yesno, done } from '@reach-sh/stdlib/ask.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const isAlice = await ask(
    `Are you Alice?`,
    yesno
  );
  const who = isAlice ? 'Alice' : 'Bob';

  console.log(`Starting Rock, Paper, Scissors! as ${who}`);

  let acc = await stdlib.newTestAccount(stdlib.parseCurrency(1000));

  let ctc = null;
  if (isAlice) {
    console.log("deploying contract...");
    ctc = acc.deploy(backend);
    const info = await ctc.getInfo();
    console.log(`The contract is deployed as = ${JSON.stringify(info)}`);
  } else {
    const info = await ask(
      `Please paste the contract information:`,
      JSON.parse
    );
    ctc = acc.attach(backend, info);
  }

  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const getBalance = async () => fmt(await stdlib.balanceOf(acc));

  const before = await getBalance();
  console.log(`Your balance is ${before}`);

  const interact = { ...stdlib.hasRandom };

  interact.informTimeout = (part_num) => {
    console.log(`There was a timeout`);
  };

  interact.informDraw = () => {
    console.log(`Draw, play again`);
  };

  const HAND = ['Rock', 'Paper', 'Scissors'];
  const getHand = async () => {
    const HANDS = {
      'Rock': 0, 'R': 0, 'r': 0,
      'Paper': 1, 'P': 1, 'p': 1,
      'Scissors': 2, 'S': 2, 's': 2,
    };

    const hand = await ask(`What hand will you play?`, (x) => {
      const hand = HANDS[x];
      if ( hand == null ) {
        throw Error(`Not a valid hand ${hand}`);
      }
      return hand;
    });
    console.log(`You played ${HAND[hand]}`);
    return hand;
  };
  interact.getHand = getHand;

  if (isAlice) {
    const amt = await ask(
      `How much do you want to wager?`,
      stdlib.parseCurrency
    );
    interact.wager = amt;
    const deadline = await ask(
      'How many blocks until a timeout?', (x) => x);
    interact.DEADLINE = deadline;
  } else {
    interact.acceptWager = async (wager, deadline) => {
      const accepted = await ask(
        `Do you accept the wager of ${fmt(wager)}? with the deadline of ${deadline} blocks`,
        yesno
      );
      if (accepted) {
        return;
      } else {
        process.exit(0);
      }
    };
  }


  const OUTCOME = ['Bob wins', 'Draw', 'Alice wins'];
  interact.seeOutcome = async (outcome) => {
    console.log(`The outcome is: ${OUTCOME[outcome]}`);
  };

  const part = isAlice ? backend.Alice : backend.Bob;
  await part(ctc, interact);

  const after = await getBalance();
  console.log(`Your balance is now ${after}`);

  done();
})();