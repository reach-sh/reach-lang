import { loadStdlib } from '@reach-sh/stdlib';
import * as stdlib from '@reach-sh/stdlib/ALGO.mjs'
import * as backend from './build/seriousrps.main.mjs';
import { ask, yesno, done } from '@reach-sh/stdlib/ask.mjs';

(async () => {
  const stdlib = await loadStdlib();

  const isDeployer = await ask(
    `Are you the deployer?`,
    yesno
  );
  const who = isDeployer ? 'Deployer' : 'Attacher';

  console.log(`Starting Rock, Paper, Scissors! as ${who}`);

  let acc = null;
  const createAcc = await ask(
    `Would you like to create an account? (only possible on devnet)`,
    yesno
  );
  if (createAcc) {
    acc = await stdlib.newTestAccount(stdlib.parseCurrency(1000));
  } else {
    const secret = await ask(
      `What is your account secret?`,
      (x => x)
    );
    acc = await stdlib.newAccountFromSecret(secret);
  }

  let ctc = null;
  if (isDeployer) {
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
    var player_timed_out = undefined;
    if (part_num === 0) {
      player_timed_out = "deployer";
    } else {
      player_timed_out = "attacher";
    }
    console.log(`timeout, ${player_timed_out} was too slow`);
  };

  interact.informDraw = () => {
    console.log(`Draw, play again`);
  };

  interact.informOpponent = (opp) => {
    console.log(opp + " joined your game!");
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

  const getBatch = async () => {
    var hands = [];
    var hand = undefined;
    for (var i = 0; i < 5; i++) {
      hand = await getHand();
      console.log(hand);
      hands.push(hand);
    }
    console.log("hands being submitted");
    console.log(hands);
    //console.log(typeof(hands));
    //console.log(typeof([1,0,0]));
    return hands;
  }

  interact.getBatch = getBatch;

  if (isDeployer) {
    const amt = await ask(
      `How much do you want to wager?`,
      stdlib.parseCurrency
    );
    interact.wager = amt;
    const deadline = await ask(
      'How many blocks until a timeout?', (x) => x);
    interact.deadline = deadline;
  } else {
    interact.acceptGame = async (wager, deadline) => {
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


  const OUTCOME = ['Attacher wins', 'Draw', 'Deployer wins'];
  interact.seeOutcome = async (outcome, dHands, aHands) => {
    console.log(`The outcome is: ${OUTCOME[outcome]}`);
    const p1_move_strs = dHands.map(x => HAND[x]);
    const p2_move_strs = aHands.map(x => HAND[x]);
    console.log('p1 moves: ');
    console.log(p1_move_strs);
    console.log('p2 moves: ');
    console.log(p2_move_strs);
  };

  const part = isDeployer ? backend.Deployer : backend.Attacher;
  await part(ctc, interact);

  const after = await getBalance();
  console.log(`Your balance is now ${after}`);

  done();
})();