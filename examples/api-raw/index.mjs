import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib(process.env);

const assertEq = (expected, actual) => {
  const exps = JSON.stringify(expected);
  const acts = JSON.stringify(actual);
  console.log('assertEq', {expected, actual}, {exps, acts});
  stdlib.assert(exps === acts) };
const {
  parseCurrency: pc,
  bigNumberify: bn,
  connector: conn,
} = stdlib;

const startingBalance = pc(100);
const [ accA, accB ] =
  await stdlib.newTestAccounts(2, startingBalance);
accA.setDebugLabel('Alice');
accB.setDebugLabel('Bob');
if (conn !== 'ALGO') { accB.setGasLimit(5_000_000); };

const run = async ({which, isRaw}) => {
  const ctcA = accA.contract(backend);
  const x = pc(2);
  let exp;
  await ctcA.p.A({
    x,
    ready: async () => {
      const [ fn, args ] = which ? [ 'f', [ x, bn(3) ] ] : [ 'g', [ x ] ];
      exp = [ x, ...args ];

      const ctcInfo = ctcA.getInfo();
      const ctc = accB.contract(backend, ctcInfo);
      let res;
      if ( ! isRaw ) {
        res = await ctc.a.P[fn](...args);
      } else if ( conn === 'ETH' || conn === 'CFX' ) {
        const { ethers } = stdlib;
        const abi = ctc.getABI();
        console.log(abi);
        const ctcRaw = new ethers.Contract(ctcInfo, abi, accB.networkAccount);
        const t = await ctcRaw[`P_${fn}`](...args, { value: x });
        console.log(t);
        res = await t.wait();
      } else {
        throw Error(`No raw mode for ${conn}`);
      }
      console.log(fn, args, '=>', res);
    },
    check: (act) => assertEq(act, exp),
  });
};

for ( const which of [ true, false ] ) {
  for ( const isRaw of [ true, false ] ) {
    await run({which, isRaw});
  }
}
