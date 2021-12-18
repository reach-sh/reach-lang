const c = await import('../../simulator-client/client.mjs');
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
  await c.respondWithVal(2,2,0,"Number")
  await c.changeActor(-1)
  await c.respondWithVal(3,3,0,"Number")
  await c.changeActor(0)
  await c.respondWithVal(4,2,-99,"Number")
  await c.changeActor(1)
  await c.respondWithVal(5,5,-99,"Number")
  await c.respondWithVal(6,6,1,"Number")
  await c.changeActor(-1)
  await c.respondWithVal(7,7,1,"Number")
  await c.changeActor(0)
  await c.respondWithVal(8,8,-99,"Number")
  await c.changeActor(1)
  await c.respondWithVal(9,9,-99,"Number")
  await c.respondWithVal(10,10,-99,"Number")
  const r = await c.getStatus()
  assert.equal(r,"Done");
  console.log("Testing Complete!")
}

main()
