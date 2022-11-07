import * as lang from '@reach-sh/simulator-lang';
import * as assert from 'assert';

const main = async () => {
  console.log("Init Testing!")
  const DRAW = 1;
  const fs = new lang.FunctionalScenario();
  let s = await fs.init();
  let alice; let bob;
  const a = fs.participants.Alice;
  const b = fs.participants.Bob;
  const consensus = fs.consensus;
  [s, alice] = await s.who(a).init(10,
    {'wager': new lang.ReachNumber(10).format(),
    'deadline': new lang.ReachNumber(99).format()}
  );
  [s, bob] = await s.who(b).init(10);
  s = await s.who(consensus).publish(alice);
  s = await s.who(bob).receive();
  s = await s.who(consensus).publish(bob);

  const play = async (s,alice,bob,consensus) => {
    let aHand = Math.floor(Math.random() * 3);
    let bHand = Math.floor(Math.random() * 3);

    s = await s.who(alice).interact('getHand', aHand);
    s = await s.who(alice).interact('random', (Math.floor(Math.random() * 4444)));
    s = await s.who(consensus).publish(alice);

    s = await s.who(alice).receive();
    s = await s.who(bob).interact('getHand', bHand);
    s = await s.who(consensus).publish(bob);
    s = await s.who(alice).receive();
    s = await s.who(bob).receive();

    s = await s.who(consensus).publish(alice);

    let outcome = (await s.who(consensus).getVar('outcome')).contents();
    // when using the FunctionalScenario, functions need to return
    // the final scenario reference
    //     ↓↓
    return [s,outcome];
  }

  let outcome = DRAW;
  let counter = 0;
  while (outcome === DRAW) {
    counter++;
    [s,outcome] = await play(s,alice,bob,consensus);
  }

  s = await s.who(alice).exit();
  s = await s.who(bob).exit();
  console.log(outcome);
  console.log(`game played ${counter} times`);
  console.log("Testing Complete!!!");
}

main();
