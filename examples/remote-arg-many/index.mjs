import {loadStdlib} from '@reach-sh/stdlib';
import * as backendClient from './build/index.mainClient.mjs';
import * as backendServer from './build/index.mainServer.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const accDeployer = await stdlib.newTestAccount(startingBalance);
const ctcServer = accDeployer.contract(backendServer);
const ctcClient = accDeployer.contract(backendClient);

const startMeUp = async (ctcDeployer, extraFields) => {
  try {
    await ctcDeployer({
      ...extraFields,
      ready: () => {
        //console.log('The contract is ready');
        throw 42;
      },
    });
  } catch (e) {
    if ( e !== 42) {
      throw e;
    }
  }
}

await startMeUp(ctcServer.p.Deployer, {})
const serverInfo = await ctcServer.getInfo()
await startMeUp(ctcClient.p.Deployer, {serverCtcInfo: serverInfo})
const clientInfo = await ctcClient.getInfo()

//const argLength = 20
const argLength = 8

const args = Array.from(new Array(argLength), (_, i) => i);

const res = await ctcClient.apis.poke(...args)
const expected = args.reduce((accum, x) => accum + x, 0)
if (stdlib.bigNumberToNumber(res) != expected) {
  throw "bad return, expected: " + expected + " got: " + stdlib.bigNumberToNumber(res)
}

