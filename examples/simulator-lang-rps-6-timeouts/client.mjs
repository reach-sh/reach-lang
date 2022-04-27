import * as lang from '@reach-sh/simulator-lang';
// import * as lang from '../../simulator/lang/lib.mjs';
import * as assert from 'assert';

const main = async () => {
  console.log("Init Testing!")

  // test functionally
  const fs = new lang.FunctionalScenario();
  await fs.init();
  let pi = await fs.pingServer();
  console.log(fs);
  let alice = fs.participants.Alice;
  let bob = fs.participants.Bob;
  let consensus = fs.consensus;
  // init Alice
  await alice.init(
    {'wager':{'tag':'V_UInt','contents':10},
    'deadline':{'tag':'V_UInt','contents':999}}
  );
  // init Bob
  await bob.init();
  // Alice interactively gets her hand (0)
  let s = await (await alice.getNextAction()).resolve(0);
  // getRandom
  s = await (await alice.getNextAction()).resolve(4444);
  // Alice's wager/deadline is published
  s = await (await s.who(consensus).getNextAction()).resolve(0);
  // Alice observes that her hand is published
  s = await (await s.who(alice).getNextAction()).resolve();
  // Bob observes that Alice's hand is published
  s = await (await s.who(bob).getNextAction()).resolve();
  // Bob interactively gets his hand (1)
  s = await (await s.who(bob).getNextAction()).resolve(1);
  // Bob's hand (1) is published
  s = await (await s.who(consensus).getNextAction()).resolve(1);
  // Alice observes that Bob's hand is published
  s = await (await s.who(alice).getNextAction()).resolve();
  // Bob observes that his hand is published
  s = await (await s.who(bob).getNextAction()).resolve();
  // Alice's hand is published
  s = await (await s.who(consensus).getNextAction()).resolve(1);
  // Seen
  s = await (await s.who(alice).getNextAction()).resolve();
  s = await (await s.who(bob).getNextAction()).resolve();
  // Done
  r = await s.who(alice).getStatus();
  console.log(r);
  assert.equal(r,"Done");
  console.log("Functional Scenario Done ########################");
  console.log("Testing Complete")
}

main();
