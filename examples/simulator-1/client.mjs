const c = await import('../../simulator-client/client.mjs');
const assert = await import('assert');

const main = async () => {
  console.log("Init Testing!")
  await c.waitForPort()
  await c.ping()
  await c.load()
  await c.init()
  await c.respondWithVal(0,0,0,"Number")
  await c.respondWithVal(1,1,"Alice","String")
  await c.respondWithVal(2,2,1,"Number")
  await c.respondWithVal(3,3,"Bob","String")
  await c.respondWithVal(4,4,0,"Number")
  await c.respondWithVal(5,5,0,"Number")
  const r = await c.getStatus()
  assert.equal(r,"Done");
  console.log("Testing Complete!")
}

// main() --
