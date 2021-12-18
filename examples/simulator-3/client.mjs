const c = await import('@reach-sh/simulator-client');
const assert = await import('assert');

const main = async () => {
  console.log("Init Testing!")
  await c.waitForPort()
  await c.ping()
  await c.load()
  await c.init()
  await c.initFor(0,0)
  await c.initFor(1,1)
  await c.changeActor(0)
  await c.respondWithVal(2,2,10)
  await c.respondWithVal(3,3,0)
  await c.respondWithVal(4,4,4444)
  await c.changeActor(-1)
  await c.respondWithVal(5,5,0)
  await c.changeActor(0)
  await c.respondWithVal(6,6,-99)
  await c.changeActor(1)
  await c.respondWithVal(7,7,-99)
  await c.respondWithVal(8,8,-99)
  await c.respondWithVal(9,9,1)
  await c.changeActor(-1)
  await c.respondWithVal(10,10,1)
  await c.changeActor(0)
  await c.respondWithVal(11,11,1)
  await c.changeActor(1)
  await c.respondWithVal(12,12,1)
  await c.changeActor(-1)
  await c.respondWithVal(13,13,0)
  await c.changeActor(0)
  await c.respondWithVal(14,14,-99)
  await c.changeActor(1)
  await c.respondWithVal(14,14,-99)
  await c.changeActor(0)
  await c.respondWithVal(15,15,-99)
  await c.changeActor(1)
  await c.respondWithVal(16,16,-99)
  const r = await c.getStatus()
  assert.equal(r,"Done");
  console.log("Testing Complete!")
}

main()
