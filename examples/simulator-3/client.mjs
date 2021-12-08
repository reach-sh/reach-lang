const c = await import('@reach-sh/simulator-client');
const assert = await import('assert');
const waitPort = await import('wait-port');

const main = async () => {
  const port = 3001
  console.log("Init Testing!")
  const params = {
    port: port
  };
  await waitPort(params)
  await c.ping()
  await c.load()
  await c.init()
  await c.respondWithVal(0,0,10,"Number")
  await c.respondWithVal(1,1,1,"Number")
  await c.respondWithVal(2,2,7,"Number")
  await c.respondWithVal(3,3,"Alice","String")
  await c.respondWithVal(4,4,0,"Number")
  await c.respondWithVal(5,5,2,"Number")
  await c.respondWithVal(6,6,"Bob","String")
  await c.respondWithVal(7,7,"Alice","String")
  await c.respondWithVal(8,8,0,"Number")
  await c.respondWithVal(9,9,0,"Number")
  const r = await c.getStatus()
  assert.equal(r,"Done");
  console.log("Testing Complete!")
}

main()
