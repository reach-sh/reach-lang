import * as lang from '@reach-sh/simulator-lang';
// import * as lang from '../../simulator/lang/lib.mjs';
import * as assert from 'assert';

const main = async () => {
  console.log("Init Testing!")

  // test functionally
  const fs = new lang.FunctionalScenario();
  let s = await fs.init();
  let pi = await fs.pingServer();
  let alice = fs.participants.Alice;
  let bob = fs.participants.Bob;
  let consensus = fs.consensus;
  // console.log("YUKBEWIULCEWIUVLELBI")
  // console.log(await s.who(consensus).getNetworkTime());
  // init Alice
  s = await s.who(alice).init(
    {'wager':{'tag':'V_UInt','contents':10},
    'deadline':{'tag':'V_UInt','contents':99}}
  );
  // init Bob
  s = await s.who(bob).init();
  // Alice interactively gets her hand (0)
  s = await (await s.who(alice).getNextAction()).resolve(0);
  // getRandom
  s = await (await s.who(alice).getNextAction()).resolve(4444);
  s = await s.wait(9999);
  // Alice's wager/deadline is published
  s = await (await s.who(consensus).getNextAction()).resolve(0);
  // Alice observes that her hand is published
  s = await (await s.who(alice).getNextAction()).resolve();
  // Bob observes that Alice's hand is published
  s = await (await s.who(bob).getNextAction()).resolve();

  // Bob interactively gets his hand (1)
  ss = await (await s.who(bob).getNextAction()).resolve(1);
  // Bob's hand (1) is published
  s = await (await ss.who(consensus).getNextAction()).resolve(1);
  // Alice observes that Bob's hand is published
  s = await (await s.who(alice).getNextAction()).resolve();
  // Bob observes that his hand is published
  s = await (await s.who(bob).getNextAction()).resolve();
  // Alice's hand is published
  s = await (await s.who(consensus).getNextAction()).resolve(0);
  // Seen
  s = await (await s.who(alice).getNextAction()).resolve();
  s = await (await s.who(bob).getNextAction()).resolve();
  // Done
  let r = await s.who(alice).getStatus();
  console.log(r);
  assert.equal(r,"Done");

  // console.log("Test Timeout Scenario ########################");

  // s = await ss.wait(9999);
  // s = await (await s.who(alice).getNextAction()).resolve();
  // let q = await s.who(consensus).getNextAction()
  // console.log(q)
  // s = await q.resolve(0);
  // s = await (await s.who(alice).getNextAction()).resolve();
  // s = await (await s.who(consensus).getNextAction()).resolve(0);
  // s = await (await s.who(alice).getNextAction()).resolve();
  // s = await (await s.who(bob).getNextAction()).resolve();
  // // Done
  // r = await s.who(alice).getStatus();
  // console.log(r);
  // assert.equal(r,"Done");

  console.log("Done ########################");
  console.log("Testing Complete")
}

main();
