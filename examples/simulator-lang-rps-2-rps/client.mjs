import * as lang from '@reach-sh/simulator-lang';
import * as assert from 'assert';

const main = async () => {
  console.log("Init Testing!")

  // test imperatively
  const s = new lang.ImperativeScenario();
  await s.init();
  const pi = await s.pingServer();
  console.log(s);
  const alice = s.participants.Alice;
  const bob = s.participants.Bob;
  const consensus = s.consensus;
  // init Alice
  await alice.init();
  // init Bob
  await bob.init();
  // Alice interactively gets her hand (0)
  await (await alice.getNextAction()).resolve(0);
  // Alice's hand (0) is published
  await (await consensus.getNextAction()).resolve(0);
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
  const r = await alice.getStatus();
  console.log(r);
  assert.equal(r,"Done");

  // test functionally


  console.log("Testing Complete")
}

main();
