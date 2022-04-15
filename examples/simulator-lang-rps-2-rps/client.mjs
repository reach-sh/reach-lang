import * as sl from '@reach-sh/simulator-lang';
import * as assert from 'assert';
import * as pj from 'prettyjson';

const logGlobal = async (n) => {
  const r = await c.getStateGlobals(n)
  console.log(`LOGGING GLOBAL STATE ${n}`)
  console.log(pj.render(r))
  console.log("-------------------")
}

const logLocal = async (n) => {
  const r = await c.getStateLocals(n)
  console.log(`LOGGING LOCAL STATE ${n}`)
  console.log(pj.render(r))
  console.log("-------------------")
}

const getVar = async (v,n,aid) => {
  const x = await c.getStateLocals(n)
  return x.l_locals[aid].l_store.find(el => el[0] == v)
}

const assertVar = (y,t,v) => {
  assert.equal(y[1].tag,t);
  assert.equal(y[1].contents,v);
}

const main = async () => {
  console.log("Init Testing!")
  const scene = new sl.Scenario();
  scene.init();
  const p = scene.pingServer();
  console.log(p);
  const alice = scene.participants[0]
  const bob = scene.participants[1]
  const consensus = scene.consensus
  // init Alice
  await alice.init();
  // init Bob
  await bob.init();
  // Alice interactively gets her hand (0)
  // await c.respondWithVal(2,2,0,0)
  await alice.getNextAction().resolve(0);
  // Alice's hand (0) is published
  // await c.respondWithVal(3,3,0,-1)
  await consensus.getNextAction().resolve(0);
  // Alice observes that her hand is published
  // await c.respondWithVal(4,2,-99,0)
  await alice.getNextAction().resolve(-4);
  // Bob observes that Alice's hand is published
  // await c.respondWithVal(5,5,-99,1)
  await bob.getNextAction().resolve(-4);
  // Bob interactively gets his hand (1)
  // await c.respondWithVal(6,6,1)
  await bob.getNextAction().resolve(1);
  // Bob's hand (1) is published
  // await c.respondWithVal(7,7,1,-1)
  await consensus.getNextAction().resolve(1);
  // Alice observes that Bob's hand is published
  // await c.respondWithVal(8,8,-99,0)
  await alice.getNextAction().resolve(-4);
  // Bob observes that his hand is published
  // await c.respondWithVal(9,9,-99,1)
  await bob.getNextAction().resolve(-4);

  const r = await alice.getStatus();
  console.log(r);
  console.log("Testing Complete")
}

main();
