import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);
const {
  parseCurrency: pc,
  bigNumberify: bn,
  connector: conn,
} = stdlib;

const startingBalance = stdlib.parseCurrency(100);
const accDeployer = await stdlib.newTestAccount(startingBalance);

const argLength = 20

const run = async (isRaw) => {


  const ctc = accDeployer.contract(backend);

  try {
    await ctc.p.Deployer({
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


  const ctcInfo = await ctc.getInfo();
  const ABI = ctc.getABI(true);
  const api = ctc.apis.A;
  const args = Array.from(new Array(argLength), (_, i) => i);
  const expected = args.reduce((accum, x) => accum + x, 0)
  const exp_res = expected

  let res = 0;


  if ( ! isRaw ) {
    res = await api.sumMany(...args)
  } else if ( conn === 'ETH' || conn === 'CFX' ) {
    // TODO
    throw "not yet implemented for ETH-like..."
  } else if ( conn === 'ALGO' ) {
    const ALGO = stdlib;
    const { algosdk } = ALGO;
    const ci = stdlib.bigNumberToNumber(ctcInfo)
    const ctcAddr = algosdk.getApplicationAddress(ci);
    const thisAcc = accDeployer.networkAccount;
    const from = thisAcc.addr;
    const params = await ALGO.getTxnParams('raw');
    const { sigs } = ABI;
    const methods = sigs.map(algosdk.ABIMethod.fromSignature);
    const meth = methods.find((x) => x.name === "A_sumMany");
    stdlib.assert(meth !== undefined, `no A_sumMany method exists`);

    const t2a = (x) => {
      if ( x._isBigNumber ) {
        return BigInt(x.toHexString());
      } else if ( x.constructor === Array ) {
        return x.map(t2a);
      } else if ( x === true ) {
        return 1;
      } else if ( x === false ) {
        return 0;
      } else {
        return x;
      }
    };
    const a2t = (x) => {
      if ( typeof x === 'bigint' ) {
        return bn(x);
      } else if ( x.constructor === Array ) {
        return x.map(a2t);
      } else if ( x === 0 ) {
        return false;
      } else if ( x === 1 ) {
        return true;
      } else {
        return x;
      }
    };

    const argsTupled = args.slice(0,14).concat([args.slice(14)])
    const margs = t2a(argsTupled);
    const eargs = meth.args.map((a, i) => a.type.encode(margs[i]));
    const sel = meth.getSelector();
    const aargs = [ sel, ...eargs ];
    const txnApp = algosdk.makeApplicationNoOpTxn(from, params, ci, aargs);
    txnApp.fee = ALGO.MinTxnFee * 2;
    const rtxns = [ txnApp ];
    algosdk.assignGroupID(rtxns);
    const wtxns = rtxns.map(ALGO.toWTxn);
    const tr = await ALGO.signSendAndConfirm( thisAcc, wtxns );
    const rlog = tr.logs.pop();
    const rlog_ui = Uint8Array.from(Buffer.from(rlog, 'base64'));
    const res_ui = rlog_ui.slice(4);
    res = meth.returns.type.decode(res_ui);
    res = a2t(res);

  } else {
    throw Error(`No raw mode for ${conn}`);
  }

  if (stdlib.bigNumberToNumber(res) != expected) {
    throw "bad return, expected: " + expected + " got: " + stdlib.bigNumberToNumber(res)
  }

}

await run(false)
await run(true)

console.log("At the end here now.  I guess we made it.")
