import * as lang from '@reach-sh/simulator-lang';
import * as assert from 'assert';

const main = async () => {
  console.log("Init Testing!")

  // test imperatively
  const impScenario = async (s,aHand,bHand) => {
    // Alice interactively gets her hand (0)
    await (await s.who(alice).getNextAction()).resolve(aHand);
    // Alice's hand (0) is published
    await (await consensus.getNextAction()).resolve(alice);
    // Alice observes that her hand is published
    await (await alice.getNextAction()).resolve();
    // Bob observes that Alice's hand is published
    await (await bob.getNextAction()).resolve();
    // Bob interactively gets his hand (1)
    await (await bob.getNextAction()).resolve(bHand);
    // Bob's hand (1) is published
    await (await consensus.getNextAction()).resolve(bob);
    // Alice observes that Bob's hand is published
    await (await alice.getNextAction()).resolve();
    // Bob observes that his hand is published
    await (await bob.getNextAction()).resolve();
    // Alice's program has run to completion
    const r = await alice.getStatus();
    console.log(r);
    assert.equal(r,"Done");
    const store = await alice.getStore();
    return store.getVar('outcome')
  }

  const winner = (aHand,bHand) => {
    return ((aHand + (4 - bHand)) % 3);
  }

  const is = new lang.ImperativeScenario();
  await is.init();
  const pi = await is.pingServer();
  console.log(is);
  const alice = is.participants.Alice;
  const bob = is.participants.Bob;
  const consensus = is.consensus;
  // init Alice
  await alice.init();
  // init Bob
  const s = await bob.init();

  for (let aHand = 0; aHand < 3; aHand++) {
    for (let bHand = 0; bHand < 3; bHand++) {
      const r = await impScenario(s.copy(),aHand,bHand);
      r.assertVar('V_UInt',winner(aHand,bHand));
    }
  }

  console.log("Testing Complete!!!")
}

main();
