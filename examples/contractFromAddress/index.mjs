import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const accAlice = await stdlib.newTestAccount(startingBalance);
const accBob = await stdlib.newTestAccount(startingBalance);

const debugPrint = false;
const d = (...args) => {
  if (debugPrint) {
    console.log(...args)
  }
}

const ctcA1 = accAlice.contract(backend);
const ctcA2 = accAlice.contract(backend);

let state = 0;

const iface = {
  getAddress: () => {
    if (state === 0) {
      state = 1;
      return accAlice.getAddress();
    } else {
      return ctcA2.getContractAddress();
    }
  },
  getAnswerKey: () => {
    if (stdlib.connector === "ALGO") {
      return [0, 0];
    } else {
      return [0, 1];
    }
  },
  ready: () => {
    d("The contract is ready.")
    throw 42;
  },
  log: d,
}

const startMeUp = async (ctc) => {
  try {
    await ctc.p.Alice(iface);
  } catch (e) {
    if ( e !== 42) {
      throw e;
    }
  }
}

await startMeUp(ctcA1);
await startMeUp(ctcA2);

const ctcBob = accBob.contract(backend, ctcA1.getInfo());
await Promise.all([ctcBob.p.Bob(iface)]);

