import { loadStdlib } from '@reach-sh/stdlib';
import * as client from './build/index.client.mjs';
import * as server from './build/index.server.mjs';
const stdlib = loadStdlib(process.env);

if (stdlib.connector === "ALGO"){
  const startingBalance = stdlib.parseCurrency(100);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const ctcClient = accAlice.contract(client);
  const ctcServer = accAlice.contract(server);

  let serverReadyResolve = false;

  const serverReadyPromise = new Promise((resolve, reject) => {
    serverReadyResolve = resolve;
  })

  const iface = {
    getInt: () => 2,
    log: console.log,
    getCtc: () => ctcServer.getInfo(),
    ready: () => {serverReadyResolve(true)},
  }

  const serverPromise = ctcServer.p.Alice(iface);
  await serverReadyPromise;
  const clientPromise = ctcClient.p.Alice(iface);
  await Promise.all([
    clientPromise,
    serverPromise,
  ]);
}
