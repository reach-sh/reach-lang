import * from '@reach-sh/simulator-client';

const main = async () => {
  const port = 3001
  console.log("Init Testing!")
  const params = {
    port: port
  };
  await waitPort(params)
  await ping()
  await load()
  await init()
  await respondWithVal(0,0,0,"Number")
  await respondWithVal(1,1,"Alice","String")
  await respondWithVal(2,2,1,"Number")
  await respondWithVal(3,3,"Bob","String")
  await respondWithVal(4,4,0,"Number")
  await respondWithVal(5,5,0,"Number")
  const r = await getStatus()
  assert.equal(r,"Done");
  console.log("Testing Complete!")
}

main()
