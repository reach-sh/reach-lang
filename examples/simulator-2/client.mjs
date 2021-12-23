const c = await import('@reach-sh/simulator-client');
const assert = await import('assert');

const log = async (n) => {
  const r = await c.getStateGlobals(n)
  console.log(`LOGGING STATE ${n}`)
  console.log(r.e_ledger)
  console.log(r.e_messages)
  console.log("-------------------")
}

const main = async () => {
  console.log("Init Testing!")
  await c.waitForPort()
  await c.ping()
  await c.load()
  await c.init()
  await c.initFor(0,0)
  await c.initFor(1,1)
  await c.respondWithVal(2,2,10,0)
  await c.respondWithVal(3,3,0)
  await c.respondWithVal(4,4,0,-1)
  await c.respondWithVal(5,5,-99,0)
  await c.respondWithVal(6,6,-99,1)
  await c.respondWithVal(7,7,-99)
  await c.respondWithVal(8,8,1)
  await c.respondWithVal(9,9,1,-1)
  await c.respondWithVal(10,10,-99,0)
  await log(10)
  await c.respondWithVal(11,11,-99,1)
  await c.respondWithVal(12,12,-99)
  const r = await c.getStatus()
  assert.equal(r,"Done");
  const l = await c.getStateGlobals(13)
  const amt = l.e_ledger["1"]["-1"]
  assert.equal(amt,10);
  console.log("Testing Complete!")
}

main()
