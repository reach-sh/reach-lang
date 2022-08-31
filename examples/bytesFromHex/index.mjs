import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const b4 = new Uint8Array([1, 255, 201, 167]); //'0x01ffc9a7'

const startingBalance = stdlib.parseCurrency(100);

const [ accAlice ] =
  await stdlib.newTestAccounts(1, startingBalance);

const ctcAlice = accAlice.contract(backend);

await Promise.all([
  backend.Alice(ctcAlice, {
    b4
  })
]);

