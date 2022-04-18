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

const startMeUp = async (ctcDeployer, extraFields) => {
  try {
    await ctcDeployer({
      ...extraFields,
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
}




{
  // remote API call

  const ctcServer = accDeployer.contract(backendServer);
  const ctcClient = accDeployer.contract(backendClient);

  await startMeUp(ctcServer.p.Deployer, {})
  const serverInfo = await ctcServer.getInfo()
  await startMeUp(ctcClient.p.Deployer, {serverCtcInfo: serverInfo})
  const clientInfo = await ctcClient.getInfo()

  const res = await ctcClient.apis.poke(...args)
  expect(stdlib.bigNumberToNumber(res))
}




{
  // Raw call to API

  const run = async (isRaw, isView) => {
    const ctc = accDeployer.contract(backendServer);
    await startMeUp(ctc.p.Deployer, {})
    const ctcInfo = await ctc.getInfo();
    const ABI = ctc.getABI(true);

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

      const ALGO = stdlib;
      const { algosdk } = ALGO;
      const ci = stdlib.bigNumberToNumber(ctcInfo)
      const ctcAddr = algosdk.getApplicationAddress(ci);
      const thisAcc = accDeployer.networkAccount;
      const from = thisAcc.addr;
      const params = await ALGO.getTxnParams('raw');
      const { sigs } = ABI;
      const methods = sigs.map(algosdk.ABIMethod.fromSignature);
      const mname = isView ? "vSumMany" : "sumMany"
      const meth = methods.find((x) => x.name === mname);
      stdlib.assert(meth !== undefined, `no ${mname} method exists`);

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
      let res = meth.returns.type.decode(res_ui);
      res = a2t(res);
      expect(res);

    } else {
      throw Error(`No raw mode for ${conn}`);
    }
  }

  await run(false, false)
  await run(false, true)
  await run(true, false)
  await run(true, true)
}



console.log("At the end here now.  I guess we made it.")
