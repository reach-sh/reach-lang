const c = await import('@reach-sh/simulator-client');
const assert = await import('assert');

const getVar = async (v,n,aid) => {
  const x = await c.getStateLocals(n)
  return x.l_locals[aid].l_store.find(el => el[0].startsWith(v))
}

const assertVar = (y,t,v) => {
  assert.equal(y[1].tag,t);
  assert.equal(JSON.stringify(y[1].contents),JSON.stringify(v));
}

const main = async () => {
  console.log("Init Testing!")
  await c.waitForPort()
  await c.resetServer()
  await c.ping()
  await c.load()
  await c.init()
  await c.initFor(0,0,JSON.stringify(
    {'wager':{'tag':'V_UInt','contents':10},
    'deadline':{'tag':'V_UInt','contents':999}}
  ))
  await c.initFor(1,1)
  await c.respondWithVal(2,2,0,-1)
  await c.respondWithVal(3,3,-99,0)
  // Alice sees her own publish
  let z = await getVar('wager',4,0)
  let m = await getVar('deadline',4,0)
  assertVar(z,'V_UInt',10)
  assertVar(m,'V_UInt',999)
  await c.respondWithVal(4,4,-99,1)
  // Bob sees Alice's publish
  z = await getVar('wager',5,1)
  m = await getVar('deadline',5,1)
  assertVar(z,'V_UInt',10)
  assertVar(m,'V_UInt',999)
  await c.respondWithVal(5,5,-99)
  await c.respondWithVal(6,6,1,-1)
  await c.respondWithVal(7,7,-99,0)
  await c.respondWithVal(8,8,0)
  await c.respondWithVal(9,9,4444)
  await c.respondWithVal(10,10,0,-1)
  await c.respondWithVal(11,11,-99,0)
  // Alice sees her own publish
  let y = await getVar('commitAlice',12,0)
  const a = {
    tag: 'V_Tuple',
    contents: [
      {tag: 'V_UInt',
       contents: 4444
      },
      {tag: 'V_UInt',
       contents: 0}
    ]}
  assertVar(y, 'V_Digest', a)
  await c.respondWithVal(12,12,-99,1)
  // Bob sees Alice's publish
  y = await getVar('commitAlice',13,1)
  assertVar(y, 'V_Digest', a)
  await c.respondWithVal(13,13,1)
  await c.respondWithVal(14,14,1,-1)
  await c.respondWithVal(15,15,-99,0)
  // Alice sees Bob's publish
  y = await getVar('handBob',16,0)
  assertVar(y,'V_UInt',1)

  await c.respondWithVal(16,16,-99,1)
  // Bob sees his own publish
  y = await getVar('handBob',17,1)
  assertVar(y,'V_UInt',1)
  await c.respondWithVal(17,17,0,-1)
  await c.respondWithVal(18,18,-99,0)
  // Alice sees her 2nd publish
  y = await getVar('saltAlice',19,0)
  z = await getVar('handAlice',19,0)
  assertVar(y, 'V_UInt', 4444)
  assertVar(z,'V_UInt',0)
  await c.respondWithVal(19,19,-99,1)
  // Bob sees Alice's 2nd publish
  y = await getVar('saltAlice',20,1)
  z = await getVar('handAlice',20,1)
  assertVar(y, 'V_UInt', 4444)
  assertVar(z,'V_UInt',0)
  await c.respondWithVal(20,20,-99)
  const l = await c.getStateGlobals(21)
  const amt = l.e_ledger["1"]["-1"]
  // Bob wins
  assert.equal(amt,10);
  const r = await c.getStatus()
  assert.equal(r,"Done");
  console.log("Testing Complete!")
}

main()
