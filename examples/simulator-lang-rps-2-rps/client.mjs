import * as lang from '@reach-sh/simulator-lang';
import * as assert from 'assert';

const main = async () => {
  console.log("Init Testing!")

  const impScenario = async (aHand,bHand) => {
    // test imperatively
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
    await bob.init();
    // Alice interactively gets her hand (0)
    await (await alice.getNextAction()).resolve(aHand);
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

  const fScenario = async (aHand,bHand) => {
    // test functionally
    const fs = new lang.FunctionalScenario();
    let s = await fs.init();
    const pi = await fs.pingServer();
    console.log(fs);
    const alice = fs.participants.Alice;
    const bob = fs.participants.Bob;
    const consensus = fs.consensus;
    // init Alice
    s = await s.who(alice).init();
    // init Bob
    s = await s.who(bob).init();
    // Alice interactively gets her hand (0)
    s = await (await s.who(alice).getNextAction()).resolve(aHand);
    // Alice's hand (0) is published
    s = await (await s.who(consensus).getNextAction()).resolve(alice);
    // Alice observes that her hand is published
    s = await (await s.who(alice).getNextAction()).resolve();
    // Bob observes that Alice's hand is published
    s = await (await s.who(bob).getNextAction()).resolve();
    // Bob interactively gets his hand (1)
    s = await (await s.who(bob).getNextAction()).resolve(bHand);
    // Bob's hand (1) is published
    s = await (await s.who(consensus).getNextAction()).resolve(bob);
    // Alice observes that Bob's hand is published
    s = await (await s.who(alice).getNextAction()).resolve();
    // Bob observes that his hand is published
    s = await (await s.who(bob).getNextAction()).resolve();
    const r = await s.who(alice).getStatus();
    console.log(r);
    assert.equal(r,"Done");
    const store = await alice.getStore();
    return store.getVar('outcome')
  }

  const winner = (aHand,bHand) => {
    return ((aHand + (4 - bHand)) % 3);
  }

  for (let aHand = 0; aHand < 3; aHand++) {
    for (let bHand = 0; bHand < 3; bHand++) {
      let r1 = await impScenario(aHand,bHand);
      let r2 = await fScenario(aHand,bHand);
      r1.assertVar('V_UInt',winner(aHand,bHand));
      r2.assertVar('V_UInt',winner(aHand,bHand));
    }
  }

  console.log("Testing Complete!!!")
}

main();
