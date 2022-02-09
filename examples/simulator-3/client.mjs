import * as c from '@reach-sh/simulator-client';
import * as  assert from 'assert';
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
  await c.initFor(0,0,JSON.stringify({'wager':{'tag':'V_UInt','contents':10}}))
  await c.initFor(1,1)
  await c.respondWithVal(2,2,0,0)
  await c.respondWithVal(3,3,4444)
  await c.respondWithVal(4,4,0,-1)
  await c.respondWithVal(5,5,-99,0)
  // Alice sees her own publish
  let y = await getVar('commitAlice',6,0)
  let z = await getVar('wager',6,0)
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
  assertVar(z,'V_UInt',10)
  await c.respondWithVal(6,6,-99,1)
  await c.respondWithVal(7,7,-99)
  // Bob sees Alice's publish
  y = await getVar('commitAlice',8,0)
  z = await getVar('wager',8,0)
  assertVar(y, 'V_Digest', a)
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
  await c.respondWithVal(12,12,0,-1)
  await c.respondWithVal(13,13,-99,0)
  // logLocal(15)
  // Alice sees her 2nd publish
  y = await getVar('saltAlice',14,0)
  z = await getVar('handAlice',14,0)
  assertVar(y, 'V_UInt', 4444)
  assertVar(z,'V_UInt',0)
  await c.respondWithVal(14,14,-99,1)
  // Bob sees Alice's 2nd publish
  y = await getVar('saltAlice',15,1)
  z = await getVar('handAlice',15,1)
  assertVar(y, 'V_UInt', 4444)
  assertVar(z,'V_UInt',0)
  await c.respondWithVal(15,15,-99,1)
  const l = await c.getStateGlobals(16)
  const amt = l.e_ledger["1"]["-1"]
  // Bob wins
  assert.equal(amt,10);
  const r = await c.getStatus()
  assert.equal(r,"Done");
  console.log("Testing Complete!")
}

main()
