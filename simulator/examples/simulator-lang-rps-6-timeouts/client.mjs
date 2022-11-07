import * as lang from '@reach-sh/simulator-lang';
import * as assert from 'assert';

const main = async () => {
  console.log("Init Testing!")

  const fs = new lang.FunctionalScenario();
  let s = await fs.init();
  let a;
  let b;
  const pi = await fs.pingServer();
  const alice = fs.participants.Alice;
  const bob = fs.participants.Bob;
  const consensus = fs.consensus;
  [s, a] = await s.who(alice).init(10,
    {'wager': new lang.ReachNumber(10).format(),
    'deadline': new lang.ReachNumber(99).format()}
  );
  [s, b] = await s.who(bob).init(10);

  const play = async (s,aHand,bHand,alice,bob,consensus) => {
    // Alice interactively gets her hand (0)
    s = await s.who(alice).interact('getHand', aHand);
    // getRandom
    s = await s.who(alice).interact('random', 4444);
    // Alice's wager/deadline is published
    s = await s.who(consensus).publish(alice);
    s = await s.who(alice).receive();
    // Bob interactively gets his hand (1)
    // let's name a special "breakpoint" that we'll return to
    // in order to test different timeout scenarios
    //  ↓↓
    let sBeforeTimeout = await s.who(bob).interact('getHand', bHand);
    // force Bob's hand publish to timeout
    s = await sBeforeTimeout.forceTimeout();
    // timeout
    s = await s.who(consensus).publish(bob);
    s = await s.who(alice).receive();
    s = await s.who(bob).receive();
    // closeTo
    s = await s.who(consensus).publish(alice);
    s = await s.who(alice).receive();
    s = await s.who(bob).receive();
    // first scenario done
    let r = await s.who(alice).getStatus();
    console.log(r);
    let w = await alice.balanceOf();
    // check that Alice kept her money
    assert.equal(w,10);

    // test the scenario where Alice times out
    // we're going back in time to our breakpoint here
    //               ↓↓
    s = await sBeforeTimeout.who(consensus).publish(bob);
    s = await s.who(alice).receive();
    s = await s.who(bob).receive();
    s = await s.forceTimeout();
    // timeout
    s = await s.who(consensus).publish(alice);
    s = await s.who(alice).receive();
    s = await s.who(bob).receive();
    // closeTo
    s = await s.who(consensus).publish(bob);
    s = await s.who(alice).receive();
    s = await s.who(bob).receive();
    w = await bob.balanceOf();
    // check that Bob got everything
    assert.equal(w,20);
  }
  await play(s,0,1,a,b,consensus);
  console.log("Testing Complete!!!")
}

main();
