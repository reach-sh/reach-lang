import * as lang from '@reach-sh/simulator-lang';
import * as assert from 'assert';

const main = async () => {
  console.log("Init Testing!")

  // test imperatively
  const impScenario = async (s,aHand,bHand,alice,bob,consensus) => {
    // Alice interactively gets her hand
    await s.who(alice).interact('getHand', aHand);
    // Alice's hand is published
    await consensus.publish(alice);
    // Bob interactively gets his hand
    await bob.interact('getHand', bHand);
    // Bob's hand is published
    await consensus.publish(bob);
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
  const [, a] = await alice.init();
  // init Bob
  const [, b] = await bob.init();

  for (let aHand = 0; aHand < 3; aHand++) {
    for (let bHand = 0; bHand < 3; bHand++) {
      const r = await impScenario(s.copy(),aHand,bHand,a,b,consensus);
      r.assertVar('V_UInt',winner(aHand,bHand));
    }
  }

  console.log("Testing Complete!!!")
}

main();
