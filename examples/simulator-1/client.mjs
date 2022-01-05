const c = await import('@reach-sh/simulator-client');
const assert = await import('assert');
const pj = await import('prettyjson')

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

const main = async () => {
  console.log("Init Testing!")
  await c.waitForPort()
  await c.resetServer()
  await c.ping()
  await c.load()
  // init consensus
  await c.init()
  // init Alice
  await c.initFor(0,0)
  // init Bob
  await c.initFor(1,1)
  // Alice interactively gets her hand (0)
  await c.respondWithVal(2,2,0,0)
  // Alice's hand (0) is published
  await c.respondWithVal(3,3,0,-1)
  // Alice observes that her hand is published
  await c.respondWithVal(4,2,-99,0)
  let y = await getVar('handAlice',5,0)
  assert.equal(y[1].tag,'V_UInt');
  assert.equal(y[1].contents,0);
  // Bob observes that Alice's hand is published
  await c.respondWithVal(5,5,-99,1)
  y = await getVar('handAlice',6,1)
  assert.equal(y[1].tag,'V_UInt');
  assert.equal(y[1].contents,0);
  // Bob interactively gets his hand (1)
  await c.respondWithVal(6,6,1)
  // Bob's hand (1) is published
  await c.respondWithVal(7,7,1,-1)
  // Alice observes that Bob's hand is published
  await c.respondWithVal(8,8,-99,0)
  y = await getVar('handBob',9,0)
  assert.equal(y[1].tag,'V_UInt');
  assert.equal(y[1].contents,1);
  // Bob observes that his hand is published
  await c.respondWithVal(9,9,-99,1)
  y = await getVar('handBob',10,1)
  assert.equal(y[1].tag,'V_UInt');
  assert.equal(y[1].contents,1);
  // Bob seeOutcome's
  await c.respondWithVal(10,10,-99)
  // Alice seeOutcome's
  await c.respondWithVal(11,11,-99, 0)

  const r = await c.getStatus()
  assert.equal(r,"Done");
  console.log("Testing Complete!")
}

main()
