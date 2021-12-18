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
  await c.respondWithVal(2,2,10,"Number")
  await c.respondWithVal(3,3,4444,"Number")
  await c.changeActor(-1)
  await c.respondWithVal(4,4,0,"Number")
  await c.changeActor(0)
  await c.respondWithVal(5,5,-99,"Number")
  await c.changeActor(1)
  await c.respondWithVal(6,6,-99,"Number")
  await c.respondWithVal(7,7,-99,"Number")
  await c.changeActor(-1)
  await c.respondWithVal(8,8,1,"Number")
  await c.changeActor(0)
  await c.respondWithVal(9,9,-99,"Number")
  await c.respondWithVal(10,10,0,"Number")
  await c.respondWithVal(11,11,0,"Number")
  await c.changeActor(-1)
  await c.respondWithVal(12,10,0,"Number")
  await c.changeActor(0)
  await c.respondWithVal(13,12,-99,"Number")
  await c.changeActor(1)
  await c.respondWithVal(14,14,-99,"Number")
  await c.respondWithVal(15,15,1,"Number")
  await c.changeActor(-1)
  await c.respondWithVal(16,13,1,"Number")
  await c.changeActor(0)
  await c.respondWithVal(17,16,-99,"Number")
  await c.changeActor(1)
  await c.respondWithVal(18,18,-99,"Number")
  await c.changeActor(-1)
  await c.respondWithVal(19,17,0,"Number")
  await c.changeActor(0)
  await c.respondWithVal(20,20,-99,"Number")
  await c.changeActor(1)
  await c.respondWithVal(21,21,-99,"Number")
  await c.respondWithVal(22,22,-99,"Number")
  const r = await c.getStatus()
  assert.equal(r,"Done");
  console.log("Testing Complete!")
}

main()
