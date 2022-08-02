import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const accAlice = await stdlib.newTestAccount(startingBalance);
const ctcAlice = accAlice.contract(backend);
await ctcAlice.p.A({
  get: [[1, 2, 3], 123]
});

const t2 = await ctcAlice.e.put.next();
const T_T2 = stdlib.T_Tuple([stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt])]);
const data = stdlib.protect(T_T2, t2.what)[0];
stdlib.assert(data[0].eq(1));
stdlib.assert(data[1].eq(123));
stdlib.assert(data[2].eq(3));

