const c = await import('@reach-sh/simulator-client');
const assert = await import('assert');

const log = async (n) => {
  const r = await c.getStateGlobals(n)
  console.log(`LOGGING STATE ${n}`)
  console.log(r.e_ledger)
  console.log(r.e_messages)
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
  await c.waitForPort()
  await c.resetServer()
  await c.ping()
  await c.load()
  await c.init()
  await c.initFor(0,0)
  await c.initFor(1,1)
  await c.respondWithVal(2,2,10,0)
  await c.respondWithVal(3,3,0)
  await c.respondWithVal(4,4,0,-1)
  await c.respondWithVal(5,5,-99,0)
  // Alice sees her own publish
  let y = await getVar('handAlice',6,0)
  let z = await getVar('wager',6,0)
  assertVar(y,'V_UInt',0)
  assertVar(z,'V_UInt',10)
  await c.respondWithVal(6,6,-99,1)
  await c.respondWithVal(7,7,-99)
  // Bob sees Alice's publish
  y = await getVar('handAlice',8,1)
  z = await getVar('wager',8,1)
  assertVar(y,'V_UInt',0)
  assertVar(z,'V_UInt',10)
  await c.respondWithVal(8,8,1)
  await c.respondWithVal(9,9,1,-1)
  await c.respondWithVal(10,10,-99,0)
  // Alice sees Bob's publish
  y = await getVar('handBob',11,0)
  assertVar(y,'V_UInt',1)
  await c.respondWithVal(11,11,-99,1)
  // Bob sees his own publish
  y = await getVar('handBob',12,1)
  assertVar(y,'V_UInt',1)
  await c.respondWithVal(12,12,-99)
  const r = await c.getStatus()
  assert.equal(r,"Done");
  const l = await c.getStateGlobals(13)
  const amt = l.e_ledger["1"]["-1"]
  // Bob wins
  assert.equal(amt,10);
  console.log("Testing Complete!")
}

main()
