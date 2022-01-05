const c = await import('@reach-sh/simulator-client');
const assert = await import('assert');

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
  await c.respondWithVal(4,4,4444)
  await c.respondWithVal(5,5,4444)
  await c.respondWithVal(6,6,0,-1)
  await c.respondWithVal(7,7,-99,0)
  await c.respondWithVal(8,8,-99,1)
  await c.respondWithVal(9,9,-99)
  await c.respondWithVal(10,10,1)
  await c.respondWithVal(11,11,1,-1)
  await c.respondWithVal(12,12,1,0)
  await c.respondWithVal(13,13,1,1)
  await c.respondWithVal(14,14,0,-1)
  await c.respondWithVal(15,15,-99,0)
  await c.respondWithVal(16,16,-99,1)
  await c.respondWithVal(17,17,-99,0)
  await c.respondWithVal(18,18,-99,1)
  const r = await c.getStatus()
  assert.equal(r,"Done");
  console.log("Testing Complete!")
}

main()
