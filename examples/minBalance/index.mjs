import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();
const startingBalance = stdlib.parseCurrency(100);

let code = 0;
const check = async (who, acc, expected) => {
  if ( stdlib.connector !== 'ALGO' ) { expected = '0'; }
  const actual = stdlib.formatCurrency(await stdlib.minimumBalanceOf(acc));
  console.log({who, expected, actual});
  if ( actual !== expected ) { code = 1; }
};

const [ accA, accB, accC ] = await stdlib.newTestAccounts(3, startingBalance);
const accD = await stdlib.createAccount();

await check('A', accA, '0.1');
await check('B', accB, '0.1');
await check('C', accC, '0.1');
await check('D', accD, '0');

const zorkmid = await stdlib.launchToken(accA, "zorkmid", "ZMD", {
  'note': Uint8Array.from(`I drink your milkshake`),
});

await check('A', accA, '0.2');

const ctcA = accA.contract(backend);
const ctcB = accB.contract(backend, ctcA.getInfo());
await Promise.all([
  ctcA.p.A({
    mid: () => check('Amid', accA, '0.4')
  }),
  ctcB.p.B({}),
]);

await check('A', accA, '0.2');
await check('B', accB, '0.1');
await check('C', accC, '0.1');
await check('D', accD, '0');
process.exit(code);
