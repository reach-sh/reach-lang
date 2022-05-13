import * as lang from '@reach-sh/simulator-lang';
import * as assert from 'assert';

const main = async () => {
  console.log("Init Testing!")

  const impScenario = async (sf,aHand,bHand,alice,bob,consensus) => {
    let s = await sf.who(alice).interact('getHand', aHand);
    s = await s.who(consensus).publish(alice);
    s = await s.who(bob).interact('getHand', bHand);
    await consensus.publish(bob);
    await alice.exit();
    await bob.exit();
    const r = await alice.getStatus();
    console.log(r);
    assert.equal(r,"Done");
    return alice.getVar('outcome');
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
  const [, a] = await alice.init();
  const [s, b] = await bob.init();

  for (let aHand = 0; aHand < 3; aHand++) {
    for (let bHand = 0; bHand < 3; bHand++) {
      const r = await impScenario(s.copy(),aHand,bHand,a,b,consensus);
      r.assertVar('V_UInt',winner(aHand,bHand));
    }
  }

  console.log("Testing Complete!!!")
}

main();
