import { loadStdlib, test } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const pc = stdlib.parseCurrency;
const bn = stdlib.bigNumberify;
const startingBalance = pc(10_000);

const [ accAlice ] =
  await stdlib.newTestAccounts(1, startingBalance);

accAlice.setGasLimit(5_000_000);

const ctcAlice = accAlice.contract(backend);

await ctcAlice.p.Alice({
  x: 5,
  amt: pc(1_000),
  isALGO: stdlib.connector == 'ALGO',
  chk: (x) => test.chk('add1', x, bn(6)) });
