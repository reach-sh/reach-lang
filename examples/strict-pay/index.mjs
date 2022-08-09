import { loadStdlib } from '@reach-sh/stdlib';
import * as clientBackend from './build/index.client.mjs';
import * as serverBackend from './build/index.server.mjs';
import { util } from '@reach-sh/stdlib';
const { thread, Signal } = util;

const stdlib = loadStdlib(process.env);
const signal = new Signal();
const startingBalance = stdlib.parseCurrency(100);

const [accTokenCreator, accServer, accClient] =
  await stdlib.newTestAccounts(3, startingBalance);

accServer.setDebugLabel(`Server Deployer`);
accClient.setDebugLabel(`Client Deployer`)

const ctcServer = accServer.contract(serverBackend);
const ctcClient = accClient.contract(clientBackend);

const tok = await stdlib.launchToken(accTokenCreator, 'Test', 'Test');
await accServer.tokenAccept(tok.id);
await accClient.tokenAccept(tok.id);
await stdlib.transfer(accTokenCreator, accClient, stdlib.parseCurrency(25), tok.id);

const sharedParams = { x: 5, y: 0, tok: tok.id };

const client = async () => {
  await signal.wait();
  const ctc = await ctcServer.getInfo();
  await clientBackend.A(ctcClient, {
    params: { ...sharedParams, ctc }
  })
}

await Promise.all([
  serverBackend.Deployer(ctcServer, {
    params: sharedParams,
    onDeployed: () => signal.notify()
  }),
  thread(client)
]);
