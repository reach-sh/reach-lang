import * as c from '@reach-sh/simulator-client';
import assert from 'assert';
import waitPort from 'wait-port';

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

main()
