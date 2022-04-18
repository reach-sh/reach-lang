import * as lang from '@reach-sh/simulator-lang';
import * as assert from 'assert';

const main = async () => {
  console.log("Init Testing!")
  const s = new lang.Scenario();
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
  await (await alice.getNextAction()).resolve(-4);
  // Bob observes that Alice's hand is published
  await (await bob.getNextAction()).resolve(-4);
  // Bob interactively gets his hand (1)
  await (await bob.getNextAction()).resolve(1);
  // Bob's hand (1) is published
  await (await consensus.getNextAction()).resolve(1);
  // Alice observes that Bob's hand is published
  await (await alice.getNextAction()).resolve(-4);
  // Bob observes that his hand is published
  await (await bob.getNextAction()).resolve(-4);
  const r = await alice.getStatus();
  console.log(r);
  assert.equal(r,"Done");
  console.log("Testing Complete")
}

main();
