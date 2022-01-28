import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib(process.env);

const assertEq = (actual, expected) => {
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

const run = async ({fn, isRaw}) => {
  const ctcA = accA.contract(backend);
  const x = pc(2);
  let exp;
  try {
  await ctcA.p.A({
    x,
    ready: async () => {
      const b13 = [
        /**/ false, true, false, true, true, false, false, true,
        ////     0     1      0     1     1      0      0      1
        /**/ false, true, false, false, true
        ////     0     1      0      0     1     0      0      0
      ];
      const b7 = [
        /**/  false, true, false, true, false, false, false
        ////     0     1      0    1  0   0  0
      ];
      const b8 = [
        /**/  false, true, false, false, true, true, false, false
        ////     0     1      0    0  1   1  0  0
      ];
      const [ isView, args, exp_res ] =
        ({ 'f': [ false, [x, bn(3)], true ],
           'g': [ false, [x], x.add(x) ],
           'h1': [ false, [x, b13], [true, bn(7), b13] ],
           'h2': [ false, [x, b7, b8], [true, bn(7), b7, b8] ],
           'v': [ true, [bn(10), bn(11)], bn(10+11).add(x) ],
        })[fn];
      exp = [ x, ...args ];
      const fnM = `${isView ? 'V' : 'P'}_${fn}`;

      const ctcInfo = await ctcA.getInfo();
      const ctc = accB.contract(backend, ctcInfo);
      const ABI = ctc.getABI(true);
      console.log(ABI);
      let res;
      if ( ! isRaw ) {
        const pfn = isView ? ctc.unsafeViews.V[fn] : ctc.a.P[fn];
        res = await pfn(...args);
      } else if ( conn === 'ETH' || conn === 'CFX' ) {
        const { ethers } = stdlib;
        const ctcRaw = new ethers.Contract(ctcInfo, ABI, accB.networkAccount);
        const iface = ctcRaw.interface;
        const t = await ctcRaw[fnM](...args, (isView ? {} : { value: x }));
        console.log({t});
        if ( isView ) {
          res = t;
        } else {
          const tr = await t.wait();
          console.log({tr});
          [ res ] = iface.parseLog(tr.logs[1]).args;
        }
      } else if ( conn === 'ALGO' ) {
        const ALGO = stdlib;
        const { algosdk } = ALGO;
        const ctcAddr = algosdk.getApplicationAddress(ctcInfo);
        const thisAcc = accB.networkAccount;
        const from = thisAcc.addr;
        const params = await ALGO.getTxnParams('raw');
        const txnPay = ALGO.makeTransferTxn(from, ctcAddr, x, null, params);
        txnPay.fee = 0;
        const { sigs } = ABI;
        const methods = sigs.map(algosdk.ABIMethod.fromSignature);
        const meth = methods.find((x) => x.name === fnM);
        stdlib.assert(meth !== undefined, `no ${fnM} method exists`);
        const t2a = (x) => {
          if ( x._isBigNumber ) {
            return BigInt(x.toHexString());
          } else if ( x.constructor === Array ) {
            return x.map(t2a);
          } else if ( x === true ) { return 1;
          } else if ( x === false ) { return 0;
          } else {
            return x;
          }
        };
        const margs = t2a(args);
        let stuff = { fn, args, exp_res, margs };
        console.log(stuff);
        const eargs = meth.args.map((a, i) => a.type.encode(margs[i]));
        const sel = meth.getSelector();
        const aargs = [ sel, ...eargs ];
        stuff = { ...stuff, eargs, sel, aargs };
        const txnApp = algosdk.makeApplicationNoOpTxn(from, params, ctcInfo, aargs);
        txnApp.fee = ALGO.MinTxnFee * 2;
        const txnPays = isView ? [] : [txnPay];
        const rtxns = [ ...txnPays, txnApp ];
        algosdk.assignGroupID(rtxns);
        const wtxns = rtxns.map(ALGO.toWTxn);
        console.log(stuff);
        const tr = await ALGO.signSendAndConfirm( thisAcc, wtxns );
        console.log({tr, ...stuff});
        const rlog = tr.logs.pop();
        console.log({rlog});
        const rlog_ui = Uint8Array.from(Buffer.from(rlog, 'base64'));
        const res_ui = rlog_ui.slice(4);
        console.log({res_ui});
        res = meth.returns.type.decode(res_ui);
        console.log({res});
        const a2t = (x) => {
          if ( typeof x === 'bigint' ) {
            return bn(x);
          } else if ( x.constructor === Array ) {
            return x.map(a2t);
          } else if ( x === 0 ) { return false;
          } else if ( x === 1 ) { return true;
          } else {
            return x;
          }
        };
        res = a2t(res);
      } else {
        throw Error(`No raw mode for ${conn}`);
      }
      assertEq(res, exp_res);
      if ( isView ) { throw 'View'; }
    },
    check: (act) => assertEq(act, exp),
  });
  } catch (e) {
    if ( e !== 'View' ) {
      throw e;
    }
  }
};

const first = true;
for ( const fn of [ 'v', 'h1', 'h2', 'f', 'g' ] ) {
for ( const isRaw of [ first, !first ] ) {
  if ( fn !== 'v' ) { continue; }
    await run({fn, isRaw});
}}
