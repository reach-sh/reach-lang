import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const accDeployer = await stdlib.newTestAccount(startingBalance);
const ctcDeployer = accDeployer.contract(backend);
const api = ctcDeployer.apis.A;

try {
  await ctcDeployer.p.Deployer({
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

const argLength = 20

const args = Array.from(new Array(argLength), (_, i) => i);

const res = await api.sumMany(...args)
const expected = args.reduce((accum, x) => accum + x, 0)
if (stdlib.bigNumberToNumber(res) != expected) {
  throw "bad return, expected: " + expected + " got: " + stdlib.bigNumberToNumber(res)
}

