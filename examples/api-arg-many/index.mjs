import {loadStdlib} from '@reach-sh/stdlib';
import * as backendClient from './build/index.mainClient.mjs';
import * as backendServer from './build/index.mainServer.mjs';
const stdlib = loadStdlib(process.env);
const {
  parseCurrency: pc,
  bigNumberify: bn,
  connector: conn,
} = stdlib;

const startingBalance = stdlib.parseCurrency(100);
const accDeployer = await stdlib.newTestAccount(startingBalance);

const argLength = 20
const args = Array.from(new Array(argLength), (_, i) => i);
const expected = args.reduce((accum, x) => accum + x, 0)

const expect = (result) => {
  if (result != expected) {
    throw "bad return, expected: " + expected + " got: " + result
  }
  return
}

const debugPrint = true;
const d = (...args) => {
  if (debugPrint) {
    console.log(...args)
  }
}

const startMeUp = async (ctcDeployer, extraFields) => {
  try {
    await ctcDeployer({
      ...extraFields,
      ready: () => {
        d("The contract is ready.")
        throw 42;
      },
    });
  } catch (e) {
    if ( e !== 42) {
      throw e;
    }
  }
}

const runRemote = async () => {
  d("Running remote API call from client contract.")

  const ctcServer = accDeployer.contract(backendServer);
  const ctcClient = accDeployer.contract(backendClient);

  d("REMOTE: Start up Server");
  await startMeUp(ctcServer.p.Deployer, {})
  const serverInfo = await ctcServer.getInfo()

  d("REMOTE: Start up Client");
  await startMeUp(ctcClient.p.Deployer, {serverCtcInfo: serverInfo})
  const clientInfo = await ctcClient.getInfo()

  d("REMOTE: Call Poke");
  const res = await ctcClient.apis.poke(...args)
  expect(stdlib.bigNumberToNumber(res))
}

const run = async (isRaw, isView) => {
  d(`Running: raw=${isRaw}, view=${isView}...`)
  const ctc = accDeployer.contract(backendServer);
  d(`Calling getABI`);
  const ABI = ctc.getABI(true);
  d(`Start me up`);
  await startMeUp(ctc.p.Deployer, {})
  const ctcInfo = await ctc.getInfo();
  const ALGO = stdlib;
  const { algosdk } = ALGO;

  if ( ! isRaw ) {
    if (isView) {
      const resMaybe = await ctc.views.vSumMany(...args)
      expect(resMaybe[1])
    } else {
      expect(await ctc.apis.sumMany(...args))
    }
  } else if ( conn === 'ETH' || conn === 'CFX' ) {
    // TODO
    throw "not yet implemented for ETH-like..."
  } else if ( conn === 'ALGO' ) {

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
    const abiEncode = (args, paramTypes) => {
      // A version of this that checks the length and does the right thing for both short and long arrays might be a useful library function...
      const args14 = args.slice(0,14)
      const argsMore = args.slice(14)
      const argsTupled = args.slice(0,14).concat([args.slice(14)])
      const ptypes14 = paramTypes.slice(0,14)
      const ptypesMore = paramTypes.slice(14)
      const ptypesTupled = ptypes14.concat([new algosdk.ABITupleType(ptypesMore)])
      const margs = t2a(argsTupled);
      return ptypesTupled.map((t, i) => t.encode(margs[i]));
    }

    const ci = stdlib.bigNumberToNumber(ctcInfo)
    const ctcAddr = algosdk.getApplicationAddress(ci);
    const thisAcc = accDeployer.networkAccount;
    const from = thisAcc.addr;
    const params = await ALGO.getTxnParams('raw');
    const { sigs } = ABI;
    const methods = sigs.map((x) => {
      d(`fromSig on '${x}'`);
      return algosdk.ABIMethod.fromSignature(x);
    });
    const mname = isView ? "vSumMany" : "sumMany"
    const meth = methods.find((x) => x.name === mname);
    stdlib.assert(meth !== undefined, `no ${mname} method exists`);
    const eargs = abiEncode(args, meth.args.map((a) => a.type))
    const sel = meth.getSelector();
    const aargs = [ sel, ...eargs ];
    d({ mname, meth, eargs, sel, aargs });
    const txnApp = algosdk.makeApplicationNoOpTxn(from, params, ci, aargs);
    txnApp.fee = ALGO.MinTxnFee * 2;
    const rtxns = [ txnApp ];
    algosdk.assignGroupID(rtxns);
    const wtxns = rtxns.map(ALGO.toWTxn);
    const tr = await ALGO.signSendAndConfirm( thisAcc, wtxns );
    const rlog = tr.logs.pop();
    const rlog_ui = Uint8Array.from(Buffer.from(rlog, 'base64'));
    const res_ui = rlog_ui.slice(4);
    let res = meth.returns.type.decode(res_ui);
    res = a2t(res);
    expect(res);

  } else {
    throw Error(`No raw mode for ${conn}`);
  }
}

if ( conn === 'ETH' || conn === 'CFX' ) {
  // Not yet supported for ETH-like
} else {
  await runRemote()
  // isRaw, isView
  await run(false, false)
  await run(false, true)
  await run(true, false)
  await run(true, true)
  d("Finished running.")
}
