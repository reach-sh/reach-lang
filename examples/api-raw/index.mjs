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
      const [ fn, args, exp_res ] =
        which ? [ 'f', [ x, bn(3) ], true ] : [ 'g', [ x ], x.add(x) ];
      exp = [ x, ...args ];

      const ctcInfo = await ctcA.getInfo();
      const ctc = accB.contract(backend, ctcInfo);
      const abi = ctc.getABI(true);
      console.log(abi);
      let res;
      if ( ! isRaw ) {
        res = await ctc.a.P[fn](...args);
      } else if ( conn === 'ETH' || conn === 'CFX' ) {
        const { ethers } = stdlib;
        const ctcRaw = new ethers.Contract(ctcInfo, abi, accB.networkAccount);
        const iface = ctcRaw.interface;
        const t = await ctcRaw[`P_${fn}`](...args, { value: x });
        console.log('t', t);
        const tr = await t.wait();
        console.log('tr', tr);
        [ res ] = iface.parseLog(tr.logs[1]).args;
      } else if ( conn === 'ALGO' ) {
        const ALGO = stdlib;
        const { algosdk } = ALGO;
        const ctcAddr = algosdk.getApplicationAddress(ctcInfo);
        const thisAcc = accB.networkAccount;
        const from = thisAcc.addr;
        const params = await ALGO.getTxnParams('raw');
        const txnPay = ALGO.makeTransferTxn(from, ctcAddr, x, null, params);
        txnPay.fee = 0;
        const txnApp = algosdk.makeApplicationNoOpTxn(from, params, ctcInfo, args);
        txnApp.fee = ALGO.MinTxnFee * 2;
        const rtxns = [ txnPay, txnApp ];
        algosdk.assignGroupID(rtxns);
        const wtxns = rtxns.map(ALGO.toWTxn);
        res = await ALGO.signSendAndConfirm( thisAcc, wtxns );
      } else {
        throw Error(`No raw mode for ${conn}`);
      }
      assertEq(res, exp_res);
    },
    check: (act) => assertEq(act, exp),
  });
};

for ( const which of [ true, false ] ) {
  for ( const isRaw of [ true, false ] ) {
    await run({which, isRaw});
  }
}
