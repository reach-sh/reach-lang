import * as lang from '@reach-sh/simulator-lang';
// import * as lang from '../../simulator/lang/lib.mjs';
import * as assert from 'assert';

const main = async () => {
  console.log("Init Testing!")

  // test imperatively
  const is = new lang.ImperativeScenario();
  await is.init();
  let pi = await is.pingServer();
  console.log(is);
  let alice = is.participants.Alice;
  let bob = is.participants.Bob;
  let consensus = is.consensus;
  // init Alice
  await alice.init();
  // init Bob
  await bob.init();
  // Alice interactively gets her hand (0)
  await (await consensus.getNextAction()).resolve(0);
  await (await alice.getNextAction()).resolve(0);
  // Alice's hand (0) is published
  // Alice observes that her hand is published
  await (await alice.getNextAction()).resolve();
  // Bob observes that Alice's hand is published
  await (await bob.getNextAction()).resolve();
  // Bob interactively gets his hand (1)
  await (await bob.getNextAction()).resolve(1);
  // Bob's hand (1) is published
  await (await consensus.getNextAction()).resolve(1);
  // Alice observes that Bob's hand is published
  await (await alice.getNextAction()).resolve();
  // Bob observes that his hand is published
  await (await bob.getNextAction()).resolve();
  // Alice's program has run to completion
  let r = await alice.getStatus();
  console.log(r);
  assert.equal(r,"Done");
  let store = await alice.getStore();
  // Alice observes that her hand is published
  store.getVar('handAlice').assertVar('V_UInt',0);
  // Alice observes Bob's hand
  store.getVar('handBob').assertVar('V_UInt',1);
  // B wins
  store.getVar('outcome').assertVar('V_UInt',0);

  console.log("Imperative Scenario Done ########################");

  // test functionally
  const fs = new lang.FunctionalScenario();
  await fs.init();
  pi = await fs.pingServer();
  console.log(fs);
  alice = fs.participants.Alice;
  bob = fs.participants.Bob;
  consensus = fs.consensus;
  // init Alice
  await alice.init();
  // init Bob
  await bob.init();
  // Alice interactively gets her hand (0)
  let s = await (await alice.getNextAction()).resolve(0);
  // Alice's hand (0) is published
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
  r = await s.who(alice).getStatus();
  console.log(r);
  assert.equal(r,"Done");
  console.log("Functional Scenario Done ########################");

  console.log("Testing Complete")
}

main();
