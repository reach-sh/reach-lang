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
  return x.l_locals[aid].l_store.find(el => el[0].startsWith(v))
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
  // init consensus
  await c.init()
  // init Alice
  await c.initFor(0,0)
  // init Bob
  await c.initFor(1,1)
  // A interact
  await c.respondWithVal(2,2,0,0)
  await c.respondWithVal(3,3,0,0)
  // A publish
  await c.respondWithVal(4,4,0,-1)
  // A observes publish
  await c.respondWithVal(5,5,-99,0)
  let y = await getVar('request',6,0)
  await logLocal(6)
  assertVar(y,'V_UInt',0)
  // B observes publish
  await c.respondWithVal(6,6,-99,1)
  y = await getVar('request',7,1)
  assertVar(y,'V_UInt',0)
  // B interact
  await c.respondWithVal(7,7,1)
  // B publish (pay)
  await c.respondWithVal(8,8,1,-1)
  // A
  await c.respondWithVal(9,9,0,0)
  // B
  await c.respondWithVal(10,10,-99,1)
  // C
  await c.respondWithVal(11,11,0,-1)
  // B
  await c.respondWithVal(12,12,-99,1)
  await c.respondWithVal(13,13,-99,1)
  const r = await c.getStatus()
  assert.equal(r,"Done");
  console.log("Testing Complete!")
}

main()
