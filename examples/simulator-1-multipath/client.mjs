const c = await import('../../simulator-client/client.mjs');
const assert = await import('assert');

const main = async () => {
  const port = 3001
  console.log("Init Testing!")
  const params = {
    port: port
  };
  await c.waitForPort(params)
  await c.load()
  await c.init()
  await c.initFor(0,0)
  await c.initFor(1,1)

  // 1ˢᵗ path
  await c.changeActor(0)
  await c.respondWithVal(2,2,0)
  await c.changeActor(-1)
  await c.respondWithVal(3,3,0)
  await c.changeActor(0)
  await c.respondWithVal(4,2,-99)
  await c.changeActor(1)
  await c.respondWithVal(5,5,-99)
  await c.respondWithVal(6,6,1)
  await c.changeActor(-1)
  await c.respondWithVal(7,7,1)
  await c.changeActor(0)
  await c.respondWithVal(8,8,-99)
  await c.changeActor(1)
  await c.respondWithVal(9,9,-99)
  await c.respondWithVal(10,10,-99)

  let r = await c.getStatus()
  assert.equal(r,"Done");

  // 2ⁿᵈ path
  await c.changeActor(0)
  await c.respondWithVal(2,2,1)
  await c.changeActor(-1)
  await c.respondWithVal(12,12,0)
  await c.changeActor(0)
  await c.respondWithVal(13,2,-99)
  await c.changeActor(1)
  await c.respondWithVal(14,14,-99)
  await c.respondWithVal(15,15,0)
  await c.changeActor(-1)
  await c.respondWithVal(16,16,1)
  await c.changeActor(0)
  await c.respondWithVal(17,17,-99)
  await c.changeActor(1)
  await c.respondWithVal(18,18,-99)
  await c.respondWithVal(19,19,-99)
  r = await c.getStatus()
  assert.equal(r,"Done");

  r = await c.getStatus()
  assert.equal(r,"Done");
  console.log("Testing Complete!")
}

main()
