import * as lang from '@reach-sh/simulator-lang';
import * as assert from 'assert';

const main = async () => {
  console.log("Init Testing!")

  const fs = new lang.FunctionalScenario();
  let s = await fs.init();
  const pi = await fs.pingServer();
  const alice = fs.participants.Alice;
  const bob = fs.participants.Bob;
  const consensus = fs.consensus;
  // init Alice
  s = await s.who(alice).init(10,
    {'wager':{'tag':'V_UInt','contents':10},
    'deadline':{'tag':'V_UInt','contents':99}}
  );
  // init Bob
  s = await s.who(bob).init(10);
  const fScenario = async (s,aHand,bHand) => {
    // Alice interactively gets her hand (0)
    s = await (await s.who(alice).getNextAction()).resolve(aHand);
    // getRandom
    s = await (await s.who(alice).getNextAction()).resolve(4444);
    // Alice's wager/deadline is published
    s = await (await s.who(consensus).getNextAction()).resolve(alice);
    // Alice observes that her hand is published
    s = await (await s.who(alice).getNextAction()).resolve();
    // Bob observes that Alice's hand is published
    s = await (await s.who(bob).getNextAction()).resolve();
    // Bob interactively gets his hand (1)
    // let's name a special "breakpoint" that we'll return to
    // in order to test different timeout scenarios
    //  ↓↓
    let sBeforeTimeout = await (await s.who(bob).getNextAction()).resolve(bHand);
    // force Bob's hand publish to timeout
    s = await sBeforeTimeout.forceTimeout();
    // timeout
    s = await (await s.who(consensus).getNextAction()).resolve(bob);
    s = await (await s.who(alice).getNextAction()).resolve();
    s = await (await s.who(bob).getNextAction()).resolve();
    // closeTo
    s = await (await s.who(consensus).getNextAction()).resolve(alice);
    s = await (await s.who(alice).getNextAction()).resolve();
    s = await (await s.who(bob).getNextAction()).resolve();
    // first scenario done
    let r = await s.who(alice).getStatus();
    console.log(r);
    let w = await alice.balanceOf();
    // check that Alice kept her money
    assert.equal(w,10);

    // test the scenario where Alice times out
    // we're going back in time to our breakpoint here
    //               ↓↓
    s = await (await sBeforeTimeout.who(consensus).getNextAction()).resolve(bob);
    s = await (await s.who(alice).getNextAction()).resolve();
    s = await (await s.who(bob).getNextAction()).resolve();
    s = await s.forceTimeout();
    // timeout
    s = await (await s.who(consensus).getNextAction()).resolve(alice);
    s = await (await s.who(alice).getNextAction()).resolve();
    s = await (await s.who(bob).getNextAction()).resolve();
    // closeTo
    s = await (await s.who(consensus).getNextAction()).resolve(alice);
    s = await (await s.who(alice).getNextAction()).resolve();
    s = await (await s.who(bob).getNextAction()).resolve();
    w = await bob.balanceOf();
    // check that Bob got everything
    assert.equal(w,20);
  }
  fScenario(s,0,1);
  console.log("Testing Complete!!!")
}

main();
