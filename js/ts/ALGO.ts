// XXX: do not import any types from algosdk; instead copy/paste them below
// XXX: can stop doing this workaround once @types/algosdk is shippable
import algosdk from 'algosdk';
import base32 from 'hi-base32';
import ethers from 'ethers';
import url from 'url';
import Timeout from 'await-timeout';

import {
  CurrencyAmount, OnProgress, WPArgs,
  debug, getDEBUG, toHex,
  isBigNumber, bigNumberify,
  setDigestWidth,
  mkAddressEq,
} from './shared';
import * as CBR from './CBR';
import {
  CBR_Null,
  CBR_Bool,
  CBR_UInt,
  CBR_Bytes,
  CBR_Address,
  CBR_Digest,
  CBR_Object,
  CBR_Data,
  CBR_Array,
  CBR_Tuple,
  CBR_Val,
} from './CBR';
import waitPort from 'wait-port';
import { labelMaps, replaceableThunk } from './shared_impl';
export * from './shared';

type BigNumber = ethers.BigNumber;
const BigNumber = ethers.BigNumber;
export const UInt_max: BigNumber =
  BigNumber.from(2).pow(64).sub(1);

// Note: if you want your programs to exit fail
// on unhandled promise rejection, use:
// node --unhandled-rejections=strict

// XXX Copy/pasted type defs from types/algosdk
// This is so that this module can be exported without our custom types/algosdk
// The unused ones are commented out
type Round = number
type Address = string
// type RawAddress = Uint8Array;
type SecretKey = Uint8Array // length 64
  // TODO: find the proper algo terminology for Wallet
type Wallet = {
    addr: Address,
    sk: SecretKey, // TODO: describe length? (64)
  }
type SignedTxn = Uint8Array;
type Txn = {
    txID: () => TxIdWrapper,
    lastRound: number,
    fee: number,
    signTxn: (sk: SecretKey) => SignedTxn,
  }
type TxnParams = {
  flatFee: boolean,
  fee: number,
  firstRound: number,
  lastRound: number,
  genesisID: number,
  genesisHash: string,
}
type StatusInfo = {
    'last-round': number,
  }
type TxIdWrapper = {
    toString: () => string
  }
type TxnInfo = {
    'confirmed-round': number,
    'application-index'?: number,
  }
// type AcctInfo = {
//     amount: number // bignumber?
//   }
type TxId = string;
type ApiCall<T> = {
  do: () => Promise<T>,
};
type CompileResultBytes = {
  src: String,
  result: Uint8Array,
  hash: Address
};

type NetworkAccount = Wallet;
type Account = {
  networkAccount: NetworkAccount,
};

type Backend = {_Connectors: {ALGO: {
  appApproval0: string,
  appApproval: string,
  appClear: string,
  ctc: string,
  steps: Array<string|null>,
  unsupported: boolean,
}}};

type Digest = BigNumber;
type SimRes = {
  prevSt: Digest,
  txns: Array<SimTxn>,
  nextSt: Digest,
  isHalt : boolean,
};
type SimTxn = {
  to: string,
  amt: BigNumber,
};

type CompiledBackend = {
  appApproval: CompileResultBytes,
  appClear: CompileResultBytes,
  ctc: CompileResultBytes,
  steps: Array<CompileResultBytes|null>,
};

type Recv = {
  didTimeout: false,
  data: Array<ContractOut>,
  value: BigNumber,
  from: string,
} | { didTimeout: true };

type ContractAttached = {
  getInfo: () => Promise<ContractInfo>,
  sendrecv: (...argz: any) => Promise<Recv>,
  recv: (...argz: any) => Promise<Recv>,
  wait: (...argz: any) => any,
  iam: (some_addr: any) => any,
};

// TODO
type ContractOut = any;

// XXX Add who the creator is to refund them
type ContractInfo = {
  getInfo?: () => Promise<ContractInfo>,
  creationRound: number,
  ApplicationID: number,
};

// ctc[ALGO] = {
//   address: string
//   appId: confirmedTxn.TransactionResults.CreatedAppIndex; // ?
//   creationRound: int // bigint?
//   logic_sig: LogicSig
//
//   // internal fields
//   // * not required to call acc.attach(bin, ctc)
//   // * required by backend
//   sendrecv: function
//   recv: function
// }

// NV = Net Value
type NV = Uint8Array;
type ALGO_Ty<BV extends CBR_Val> = {
  name: string,
  canonicalize: (uv: unknown) => BV,
  netSize: number // in bytes
         | 'all', // XXX
  toNet(bv: BV): NV,
  fromNet(nv: NV): BV,
}
type AnyALGO_Ty = ALGO_Ty<CBR_Val>;

// const V_Null: CBR_Null = null;
export const T_Null: ALGO_Ty<CBR_Null> = {
  ...CBR.BT_Null,
  netSize: 0,
  toNet: (bv: CBR_Null): NV => (void(bv), new Uint8Array([])),
  fromNet: (nv: NV): CBR_Null => (void(nv), null),
}

export const T_Bool: ALGO_Ty<CBR_Bool> = {
  ...CBR.BT_Bool,
  netSize: 1,
  toNet: (bv: CBR_Bool): NV => (new Uint8Array(
    [bv ? 1 : 0]
  )),
  fromNet: (nv: NV): CBR_Bool => {
    return (nv[0] === 1);
  },
}
// const V_Bool = (val: boolean): CBR_Bool => T_Bool.canonicalize(val);

export const T_UInt: ALGO_Ty<CBR_UInt> = {
  ...CBR.BT_UInt,
  netSize: 8, // UInt64
  toNet: (bv: CBR_UInt): NV => (
    ethers.utils.zeroPad(ethers.utils.arrayify(bv), 8)
  ),
  fromNet: (nv: NV): CBR_UInt => {
    // debug(`fromNet: UInt`);
    // if (getDEBUG()) console.log(nv);
    return ethers.BigNumber.from(nv);
  },
}
// const V_UInt = (n: BigNumber): CBR_UInt => {
//   return T_UInt.canonicalize(n);
// }

/** @description For arbitrary utf8 strings */
const stringyNet = {
  toNet: (bv: CBR_Bytes): NV => (
    ethers.utils.toUtf8Bytes(bv)
  ),
  fromNet: (nv: NV): CBR_Bytes => (
    ethers.utils.toUtf8String(nv)
  ),
}

/** @description For hex strings representing bytes */
const bytestringyNet = {
  toNet: (bv: string): NV => (
    ethers.utils.arrayify(bv)
  ),
  fromNet: (nv: NV): string => (
    ethers.utils.hexlify(nv)
  )
}

export const T_Bytes: ALGO_Ty<CBR_Bytes> = {
  ...CBR.BT_Bytes,
  ...stringyNet,
  netSize: 'all', // XXX
}
// const V_Bytes = (s: string): CBR_Bytes => {
//   return T_Bytes.canonicalize(s);
// }

export const T_Digest: ALGO_Ty<CBR_Digest> = {
  ...CBR.BT_Digest,
  ...bytestringyNet,
  netSize: 32,
}
// /** @description You probably don't want to manually create this */
// const V_Digest = (s: string): CBR_Digest => {
//   return T_Digest.canonicalize(s);
// }

function addressUnwrapper(x: any): string {
  return (x && x.addr)
    ? '0x' + Buffer.from(algosdk.decodeAddress(x.addr).publicKey).toString('hex')
    : x;
}
export const T_Address: ALGO_Ty<CBR_Address> = {
  ...CBR.BT_Address,
  ...bytestringyNet,
  netSize: 32,
  canonicalize: (uv: unknown): CBR_Address => {
    const val = addressUnwrapper(uv);
    return CBR.BT_Address.canonicalize(val || uv)
  }
}
// const V_Address = (s: string): CBR_Address => {
//   return T_Address.canonicalize(s);
// }

export const T_Array = (
  co: ALGO_Ty<CBR_Val>,
  size: number,
): ALGO_Ty<CBR_Array> => ({
  ...CBR.BT_Array(co, size),
  netSize: (co.netSize === 'all') ? 'all' : size * co.netSize,
  toNet: (bv: CBR_Array): NV => {
    return ethers.utils.concat(bv.map((v) => co.toNet(v)));
  },
  fromNet: (nv: NV): CBR_Array => {
    if (co.netSize === 'all') {
      // XXX there can only be one
      return [co.fromNet(nv)];
    } else {
      const chunks = new Array(size).fill(null);
      let rest = nv;
      for (const i in chunks) {
        chunks[i] = co.fromNet(rest.slice(0, co.netSize));
        rest = rest.slice(co.netSize);
      }
      // TODO: assert size of nv/rest is correct?
      return chunks;
    }
  },
});
// const V_Array = (
//   co: ALGO_Ty<CBR_Val>,
//   size: number,
// ) => (val: Array<unknown>): CBR_Array => {
//   return T_Array(co, size).canonicalize(val);
// }

export const T_Tuple = (
  cos: Array<ALGO_Ty<CBR_Val>>,
): ALGO_Ty<CBR_Tuple> => ({
  ...CBR.BT_Tuple(cos),
  netSize: (
    (cos.some((co) => co.netSize === 'all'))
    ? 'all'
    // @ts-ignore // ts should know this from the condition
    : cos.reduce((acc, co) => acc + co.netSize, 0)
  ),
  toNet: (bv: CBR_Tuple): NV => {
    const val = cos.map((co, i) => co.toNet(bv[i]));
    return ethers.utils.concat(val);
  },
  // TODO: share more code w/ T_Array.fromNet
  fromNet: (nv: NV): CBR_Tuple => {
    const chunks: Array<CBR_Val> = new Array(cos.length).fill(null);
    let rest = nv;
    for (const i in cos) {
      const co = cos[i];
      if (co.netSize === 'all') {
        // XXX consumes it all
        chunks[i] = co.fromNet(rest);
        rest = rest.slice(rest.length);
      } else {
        chunks[i] = co.fromNet(rest.slice(0, co.netSize));
        rest = rest.slice(co.netSize);
      }
    }
    return chunks;
  },
});

export const T_Object = (
  coMap: {[key: string]: ALGO_Ty<CBR_Val>}
): ALGO_Ty<CBR_Object> => {
  const cos = Object.values(coMap);
  const netSize = cos.some((co) => co.netSize === 'all')
      ? 'all'
      // @ts-ignore // ts should know this from the condition
      : cos.reduce((acc, co) => acc + co.netSize, 0);
  const {ascLabels} = labelMaps(coMap);
  return {
    ...CBR.BT_Object(coMap),
    netSize,
    toNet: (bv: CBR_Object): NV => {
      const chunks = ascLabels.map((label) =>
        coMap[label].toNet(bv[label])
      );
      return ethers.utils.concat(chunks);
    },
    // TODO: share more code w/ T_Array.fromNet and T_Tuple.fromNet
    fromNet: (nv: NV): CBR_Object => {
      const obj: {[key: string]: CBR_Val} = {};
      let rest = nv;
      for (const iStr in ascLabels) {
        const i = parseInt(iStr);
        const label = ascLabels[i];
        const co = coMap[label];
        if (co.netSize === 'all') {
          // XXX consumes it all
          obj[label] = co.fromNet(rest);
          rest = rest.slice(rest.length);
        } else {
          obj[label] = co.fromNet(rest.slice(0, co.netSize));
          rest = rest.slice(co.netSize);
        }
      }
      return obj;
    },
  }
};

// 1 byte for the label
// the rest right-padded with zeroes
// up to the size of the largest variant
export const T_Data = (
  coMap: {[key: string]: ALGO_Ty<CBR_Val>}
): ALGO_Ty<CBR_Data> => {
  const cos = Object.values(coMap);
  const valSize = cos.some((co) => co.netSize === 'all')
      ? 'all'
      // @ts-ignore // ts should know this from the cond above
      : Math.max(cos.map((co) => co.netSize))
  const netSize = valSize === 'all'
      ? 'all' : valSize + 1;
  const {ascLabels, labelMap} = labelMaps(coMap);
  return {
    ...CBR.BT_Data(coMap),
    netSize,
    toNet: ([label, val]: CBR_Data): NV => {
      const i = labelMap[label];
      const lab_nv = new Uint8Array([i]);
      const val_co = coMap[label];
      const val_nv = val_co.toNet(val);
      if (valSize === 'all') {
        return ethers.utils.concat([lab_nv, val_nv]);
      } else {
        const padding = new Uint8Array(valSize - val_nv.length);
        return ethers.utils.concat([lab_nv, val_nv, padding]);
      }
    },
    fromNet: (nv: NV): CBR_Data => {
      const i = nv[0];
      const label = ascLabels[i];
      const val_co = coMap[label];
      const rest = nv.slice(1);
      const sliceTo = val_co.netSize === 'all'
          ? rest.length : val_co.netSize;
      const val = val_co.fromNet(rest.slice(0, sliceTo));
      return [label, val];
    },
  }
}

// Common interface exports

// TODO: read token from scripts/algorand-devnet/algorand_data/algod.token
const token = process.env.ALGO_TOKEN || 'c87f5580d7a866317b4bfe9e8b8d1dda955636ccebfa88c12b414db208dd9705';
const server = process.env.ALGO_SERVER || 'http://localhost';
const port = process.env.ALGO_PORT || 4180;
const [getAlgodClient, setAlgodClient] = replaceableThunk(async () => {
  await wait1port(server, port);
  return new algosdk.Algodv2(token, server, port);
});
export {setAlgodClient};

const itoken = process.env.ALGO_INDEXER_TOKEN || 'reach-devnet';
const iserver = process.env.ALGO_INDEXER_SERVER || 'http://localhost';
const iport = process.env.ALGO_INDEXER_PORT || 8980;
const [getIndexer, setIndexer] = replaceableThunk(async () => {
  await wait1port(iserver, iport);
  return new algosdk.Indexer(itoken, iserver, iport);
});
export {setIndexer};

// eslint-disable-next-line max-len
const FAUCET = algosdk.mnemonicToSecretKey((process.env.ALGO_FAUCET_PASSPHRASE || 'pulp abstract olive name enjoy trick float comfort verb danger eternal laptop acquire fetch message marble jump level spirit during benefit sure dry absent history'));
// if using the default:
// assert(FAUCET.addr === 'EYTSJVJIMJDUSRRNTMVLORTLTOVDWZ6SWOSY77JHPDWSD7K3P53IB3GUPQ');

// Helpers

async function wait1port(theServer: string, thePort: string | number) {
  thePort = typeof thePort === 'string' ? parseInt(thePort, 10) : thePort;
  const {hostname} = url.parse(theServer);
  const args: WPArgs = {
    host: hostname || undefined,
    port: thePort,
    output: 'silent',
    timeout: 1000 * 60 * 1,
  }
  debug('wait1port');
  if (getDEBUG()) {
    console.log(args)
  }
  debug('waitPort complete');
  return await waitPort(args);
}

const getLastRound = async (): Promise<Round> =>
  (await (await getAlgodClient()).status().do())['last-round'];

const waitForConfirmation = async (txId: TxId, untilRound: number): Promise<TxnInfo> => {
  const algodClient = await getAlgodClient();
  let lastRound: null | number = null;
  do {
    const lastRoundAfterCall: ApiCall<StatusInfo> = lastRound ?
      algodClient.statusAfterBlock(lastRound) :
      algodClient.status();
    lastRound = (await lastRoundAfterCall.do())['last-round'];
    const pendingInfo =
      await algodClient.pendingTransactionInformation(txId).do();
    const confirmedRound = pendingInfo['confirmed-round'];
    if (confirmedRound && confirmedRound > 0) {
      return pendingInfo;
    }
  } while (lastRound < untilRound);

  throw { type: 'waitForConfirmation', txId, untilRound, lastRound };
};

const sendAndConfirm = async (
  stx_or_stxs: SignedTxn | Array<SignedTxn>, txn: Txn
): Promise<TxnInfo> => {
  const txID = txn.txID().toString();
  const untilRound = txn.lastRound;
  const req = (await getAlgodClient()).sendRawTransaction(stx_or_stxs);
  // @ts-ignore XXX
  debug(`sendAndConfirm: ${base64ify(req.txnBytesToPost)}`);
  try {
    await req.do();
  } catch (e) {
    throw { type: 'sendRawTransaction', e };
  }
  return await waitForConfirmation(txID, untilRound);
};

// // Backend
const compileTEAL = async (label: string, code: string): Promise<CompileResultBytes> => {
  debug(`compile ${label}`)
  let s, r;
  try {
    r = await (await getAlgodClient()).compile(code).do();
    s = 200;
  } catch (e) {
    s = typeof e === 'object' ? e.statusCode : 'not object';
    r = e;
  }

  if ( s == 200 ) {
    debug(`compile ${label} succeeded: ${JSON.stringify(r)}`);
    r.src = code;
    r.result = new Uint8Array(Buffer.from(r.result, "base64"));
    // debug(`compile transformed: ${JSON.stringify(r)}`);
    return r;
  } else {
    throw Error(`compile ${label} failed: ${s}: ${JSON.stringify(r)}`);
  }
};

const getTxnParams = async (): Promise<TxnParams> => {
  debug(`fillTxn: getting params`);
  while (true) {
    const params = await (await getAlgodClient()).getTransactionParams().do();
    debug(`fillTxn: got params: ${JSON.stringify(params)}`);
    if (params.firstRound !== 0) {
      return params;
    }
    debug(`...but firstRound is 0, so let's wait and try again.`);
    // Assumption: firstRound will move past 0 on its own.
    await Timeout.set(1000);
  }
};

const sign_and_send_sync = async (
  label: string,
  sk: SecretKey,
  txn: Txn,
): Promise<TxnInfo> => {
  const txn_s = txn.signTxn(sk);
  try {
    return await sendAndConfirm(txn_s, txn);
  } catch (e) {
    throw Error(`${label} txn failed:\n${JSON.stringify(txn)}\nwith:\n${JSON.stringify(e)}`);
  }
};

// const fillTxn = async (round_width, txn) => {
//   return fillTxnWithParams(false, round_width, await getTxnParams(), txn);
// };

export const transfer = async (from: Account, to: Account, value: BigNumber): Promise<TxnInfo> => {
  const valuen = value.toNumber();
  const sender = from.networkAccount;
  const receiver = to.networkAccount.addr;

  const note = algosdk.encodeObj('@reach-sh/ALGO.mjs transfer');
  return await sign_and_send_sync(
    `transfer ${JSON.stringify(from)} ${JSON.stringify(to)} ${valuen}`,
    sender.sk,
    algosdk.makePaymentTxnWithSuggestedParams(
      sender.addr, receiver, valuen, undefined, note, await getTxnParams(),
    ));
};

// XXX I'd use x.replaceAll if I could (not supported in this node version), but it would be better to extend ConnectorInfo so these are functions
const replaceAll = (orig: string, what: string, whatp: string): string => {
  const once = orig.replace(what, whatp);
  if ( once === orig ) {
    return orig;
  } else {
    return replaceAll(once, what, whatp);
  }
};

const replaceUint8Array = (label: string, arr: Uint8Array, x:string): string =>
  replaceAll(x, `"{{${label}}}"`, `base32(${base32.encode(arr).toString()})`);

const replaceAddr = (label: string, addr: Address, x:string): string =>
  replaceUint8Array(label, algosdk.decodeAddress(addr).publicKey, x);

function must_be_supported(bin: Backend) {
  const algob = bin._Connectors.ALGO;
  const { unsupported } = algob;
  if ( unsupported ) {
    throw Error(`This Reach application is not supported on Algorand.`);
  }
}

async function compileFor(bin: Backend, ApplicationID: number): Promise<CompiledBackend> {
  must_be_supported(bin);
  const algob = bin._Connectors.ALGO;

  const { appApproval, appClear, ctc, steps } = algob;

  const subst_appid = (x: string) =>
    replaceUint8Array(
      'ApplicationID',
      // @ts-ignore XXX
      T_UInt.toNet(bigNumberify(ApplicationID)),
      x);

  const ctc_bin = await compileTEAL('ctc_subst', subst_appid(ctc));
  const subst_ctc = (x: string) =>
    replaceAddr('ContractAddr', ctc_bin.hash, x);

  let appApproval_subst = appApproval;
  const stepCode_bin: Array<CompileResultBytes|null> =
    await Promise.all(steps.map(async (mc, mi) => {
      if ( !mc ) { return null; }
      const mN = `m${mi}`;
      const mc_subst = subst_ctc(subst_appid(mc));
      const cr = await compileTEAL(mN, mc_subst);
      appApproval_subst =
        replaceAddr(mN, cr.hash, appApproval_subst);
      return cr;
  }));

  const appApproval_bin =
    await compileTEAL('appApproval_subst', appApproval_subst);
  const appClear_bin =
    await compileTEAL('appClear', appClear);

  return { appApproval: appApproval_bin,
    appClear: appClear_bin,
    ctc: ctc_bin,
    steps: stepCode_bin,
  };
};

const ui8z = new Uint8Array();

const base64ify = (x: any): String => Buffer.from(x).toString('base64');

const format_failed_request = (e: any) => {
  const ep = JSON.parse(JSON.stringify(e));
  const db64 =
    ep.req ?
    (ep.req.data ? base64ify(ep.req.data) :
     `no data, but ${JSON.stringify(Object.keys(ep.req))}`) :
     `no req, but ${JSON.stringify(Object.keys(ep))}`;
  const msg = e.text ? JSON.parse(e.text) : e;
  return `\n${db64}\n${JSON.stringify(msg)}`;
};

const doQuery = async (dhead:string, query: any): Promise<any> => {
  //debug(`${dhead} --- QUERY = ${JSON.stringify(query)}`);
  let res;
  try {
    res = await query.do();
  } catch (e) {
    throw Error(`${dhead} --- QUERY FAIL: ${JSON.stringify(e)}`);
  }

  if ( res.transactions.length == 0 ) {
    // debug(`${dhead} --- RESULT = empty`);
    // XXX Look at the round in res and wait for a new round
    return null;
  }

  debug(`${dhead} --- RESULT = ${JSON.stringify(res)}`);
  // @ts-ignore XXX
  const txn = res.transactions[0];

  return txn;
};

const argsSlice = <T>(args: Array<T>, cnt: number): Array<T> =>
  cnt == 0 ? [] : args.slice(-1 * cnt);

export const connectAccount = async (networkAccount: NetworkAccount) => {
  // XXX become the monster
  setDigestWidth(8);

  const indexer = await getIndexer();
  const thisAcc = networkAccount;
  const shad = thisAcc.addr.substring(2, 6);
  const pk = algosdk.decodeAddress(thisAcc.addr).publicKey;
  const pks = T_Address.canonicalize(thisAcc);
  debug(`${shad}: connectAccount`);

  const iam = (some_addr: string): string => {
    const pks = '0x' + Buffer.from(pk).toString('hex');
    if (some_addr == pks) {
      return some_addr;
    } else {
      throw Error(`I should be ${some_addr}, but am ${pks}`);
    }
  };

  const attachP = async (bin: Backend, ctcInfoP: Promise<ContractInfo>): Promise<ContractAttached> => {
    const ctcInfo = await ctcInfoP;
    const getInfo = async () => ctcInfo;
    const ApplicationID = ctcInfo.ApplicationID;
    let lastRound = ctcInfo.creationRound;
    debug(`${shad}: attach ${ApplicationID} created at ${lastRound}`);

    const bin_comp = await compileFor(bin, ApplicationID);
    // XXX call verify
    const ctc_prog = algosdk.makeLogicSig(bin_comp.ctc.result, []);

    const wait = async (delta: BigNumber): Promise<BigNumber> => {
      return await waitUntilTime(bigNumberify(lastRound).add(delta));
    };

    const sendrecv = async (
      label: string,
      funcNum: number,
      evt_cnt: number,
      tys: Array<AnyALGO_Ty>,
      args: Array<any>,
      value: BigNumber,
      out_tys: Array<AnyALGO_Ty>,
      timeout_delay: undefined | BigNumber,
      sim_p: (fake: Recv) => SimRes,
    ): Promise<Recv> => {
      const funcName = `m${funcNum}`;
      const dhead = `${shad}: ${label} sendrecv ${funcName} ${timeout_delay}`;
      debug(`${dhead} --- START`);

      const handler = bin_comp.steps[funcNum];
      if ( ! handler ) {
        throw Error(`${dhead} Internal error: reference to undefined handler: ${funcName}`); }

      const fake_res = {
        didTimeout: false,
        data: argsSlice(args, evt_cnt),
        value: value,
        from: pks,
      };
      const sim_r = sim_p( fake_res );
      debug(`${dhead} --- SIMULATE ${JSON.stringify(sim_r)}`);
      const isHalt = sim_r.isHalt;
      const sim_txns = sim_r.txns;

      while ( true ){
        const params = await getTxnParams();
        if ( timeout_delay ) {
          const tdn = timeout_delay.toNumber();
          params.lastRound = lastRound + tdn;
          if ( params.firstRound > params.lastRound ) {
            debug(`${dhead} --- FAIL/TIMEOUT`);
            return {didTimeout: true};
          }
        }

        debug(`${dhead} --- ASSEMBLE w/ ${JSON.stringify(params)}`);

        const txnFromContracts =
          sim_txns.map(
            (txn_nfo: SimTxn) =>
            algosdk.makePaymentTxnWithSuggestedParams(
              bin_comp.ctc.hash,
              // XXX use some other function
              algosdk.encodeAddress(Buffer.from(txn_nfo.to.slice(2), 'hex')),
              txn_nfo.amt.toNumber(),
              undefined, ui8z,
              params));
        const totalFromFee =
          txnFromContracts.reduce(((sum: number, txn: Txn): number => sum + txn.fee), 0);
        debug(`${dhead} --- totalFromFee = ${JSON.stringify(totalFromFee)}`);

        debug(`${dhead} --- isHalt = ${JSON.stringify(isHalt)}`);

        const actual_args =
        [ sim_r.prevSt, sim_r.nextSt, isHalt, bigNumberify(totalFromFee), lastRound, ...args ];
      const actual_tys =
        [ T_Digest, T_Digest, T_Bool, T_UInt, T_UInt, ...tys ]; //.map(oldify);
      debug(`${dhead} --- ARGS = ${JSON.stringify(actual_args)}`);

      const safe_args: Array<NV> = actual_args.map((m, i) => actual_tys[i].toNet(m));
      safe_args.forEach((x) => {
        if (! ( x instanceof Uint8Array ) ) {
          // The types say this is impossible now,
          // but we'll leave it in for a while just in case...
          throw Error(`expect safe program argument, got ${JSON.stringify(x)}`);
        }
      });

      debug(`${dhead} --- PREPARE: ${JSON.stringify(safe_args.map(toHex))}`);
      const handler_with_args =
        algosdk.makeLogicSig(handler.result, safe_args);
      debug(`${dhead} --- PREPARED`); // XXX display handler_with_args usefully, like with base64ify toBytes

        const whichAppl =
          isHalt ?
          // We are treating it like any party can delete the application, but the docs say it may only be possible for the creator. The code appears to not care: https://github.com/algorand/go-algorand/blob/0e9cc6b0c2ddc43c3cfa751d61c1321d8707c0da/ledger/apply/application.go#L589
          algosdk.makeApplicationDeleteTxn :
          algosdk.makeApplicationNoOpTxn;
        // XXX if it is a halt, generate closeremaindertos for all the handlers and the contract account
        const txnAppl =
          whichAppl(
            thisAcc.addr, params, ApplicationID);
        const txnFromHandler =
          algosdk.makePaymentTxnWithSuggestedParams(
            handler.hash, 
            thisAcc.addr,
            0, undefined, ui8z,
            params);
        debug(`${dhead} --- txnFromHandler = ${JSON.stringify(txnFromHandler)}`);
        const txnToHandler =
          algosdk.makePaymentTxnWithSuggestedParams(
            thisAcc.addr,
            handler.hash,
            txnFromHandler.fee,
            undefined, ui8z,
            params);
        debug(`${dhead} --- txnToHandler = ${JSON.stringify(txnToHandler)}`);
        const txnToContract =
          algosdk.makePaymentTxnWithSuggestedParams(
            thisAcc.addr,
            bin_comp.ctc.hash,
            value.toNumber() + totalFromFee,
            undefined, ui8z,
            params);
        const txns = [
          txnAppl,
          txnToHandler,
          txnFromHandler,
          txnToContract,
          ...txnFromContracts ];
        algosdk.assignGroupID(txns);

        const sign_me = (x: Txn) => x.signTxn(thisAcc.sk);

        const txnAppl_s = sign_me(txnAppl);
        const txnFromHandler_s =
          algosdk.signLogicSigTransactionObject(
            txnFromHandler, handler_with_args).blob;
        debug(`txnFromHandler_s: ${base64ify(txnFromHandler_s)}`);
        const txnToHandler_s = sign_me(txnToHandler);
        const txnToContract_s = sign_me(txnToContract);
        const txnFromContracts_s =
          txnFromContracts.map(
            (txn: Txn) =>
            algosdk.signLogicSigTransactionObject(txn, ctc_prog).blob);

        const txns_s = [
          txnAppl_s,
          txnToHandler_s,
          txnFromHandler_s,
          txnToContract_s,
          ...txnFromContracts_s
        ];

        debug(`${dhead} --- SEND: ${txns_s.length}`);
        let res;
        try {
          res = await sendAndConfirm( txns_s, txnAppl );
        } catch (e) {
          if ( e.type == "sendRawTransaction" ) {
            throw Error(`${dhead} --- FAIL:\n${format_failed_request(e.e)}`);
          } else {
            throw Error(`${dhead} --- FAIL:\n${JSON.stringify(e)}`);
          }
        }

        // XXX we should inspect res and if we failed because we didn't get picked out of the queue, then we shouldn't error, but should retry and let the timeout logic happen.
        debug(`${dhead} --- SUCCESS: ${JSON.stringify(res)}`);

        return await recv(label, funcNum, evt_cnt, out_tys, timeout_delay);
      }
    };

    const recv = async (
      label: string,
      funcNum: number,
      evt_cnt: number,
      tys: Array<AnyALGO_Ty>,
      timeout_delay: undefined | BigNumber
    ): Promise<Recv> => {
      const funcName = `m${funcNum}`;
      const dhead = `${shad}: ${label} recv ${funcName} ${timeout_delay}`;
      debug(`${dhead} --- START`);

      const handler = bin_comp.steps[funcNum];
      if ( ! handler ) {
        throw Error(`${dhead} Internal error: reference to undefined handler: ${funcName}`); }

      const timeoutRound =
        timeout_delay ?
        lastRound + timeout_delay.toNumber() :
        undefined;

      while ( true ) {
        const currentRound = await getLastRound();
        if ( timeoutRound && timeoutRound < currentRound ) {
          return { didTimeout: true };
        }

        let query = indexer.searchForTransactions()
          .address(handler.hash)
          .addressRole("sender")
          .minRound(lastRound);
        if ( timeoutRound ) {
          query = query.maxRound(timeoutRound);
        }

        const txn = await doQuery(dhead, query);
        if ( ! txn ) {
          // XXX perhaps wait until a new round has happened using wait
          await Timeout.set(2000);
          continue;
        }

        const ctc_args: Array<string> =
          // @ts-ignore XXX
          txn.signature.logicsig.args;
        debug(`${dhead} --- ctc_args = ${JSON.stringify(ctc_args)}`);

        const args = argsSlice(ctc_args, evt_cnt);
        debug(`${dhead} --- args = ${JSON.stringify(args)}`);

        /** @description base64->hex->arrayify */
        const reNetify = (x: string): NV => {
          const s: string = Buffer.from(x, 'base64').toString('hex');
          // debug(`${dhead} --- deNetify ${s}`);
          return ethers.utils.arrayify('0x' + s);
        }

        const args_un =
            args.map((x, i) => tys[i].fromNet(reNetify(x)));
        debug(`${dhead} --- args_un = ${JSON.stringify(args_un)}`);

        const totalFromFee =
          T_UInt.fromNet(reNetify(ctc_args[3]));
        debug(`${dhead} --- totalFromFee = ${JSON.stringify(totalFromFee)}`);

        const fromAddr =
          txn['payment-transaction'].receiver;
        const from =
          T_Address.canonicalize({addr: fromAddr});
        debug(`${dhead} --- from = ${JSON.stringify(from)} = ${fromAddr}`);

        const oldLastRound = lastRound;
        lastRound = txn['confirmed-round'];
        debug(`${dhead} --- updating round from ${oldLastRound} to ${lastRound}`);

        // XXX ideally we'd get the whole transaction group before and not need to do this.
        const ptxn =
          await doQuery(
            dhead,
            indexer.searchForTransactions()
              .address(bin_comp.ctc.hash)
              .addressRole("receiver")
              .round(lastRound));

        const value =
          bigNumberify(ptxn['payment-transaction'].amount)
            .sub(totalFromFee);
        debug(`${dhead} --- value = ${JSON.stringify(value)}`);

        return {
          didTimeout: false,
          data: args_un,
          value, from,
        };
      }
    };

    return { getInfo, sendrecv, recv, iam, wait };
  };

  const deployP = async (bin: Backend): Promise<ContractAttached> => {
    must_be_supported(bin);
    debug(`${shad} deploy`);
    const algob = bin._Connectors.ALGO;

    const { appApproval0, appClear } = algob;

    const appApproval0_subst =
      replaceAddr('Deployer', thisAcc.addr, appApproval0);
    const appApproval0_bin =
      await compileTEAL('appApproval0', appApproval0_subst);
    const appClear_bin =
      await compileTEAL('appClear', appClear);

    const createRes =
      await sign_and_send_sync(
        'ApplicationCreate',
        thisAcc.sk,
        algosdk.makeApplicationCreateTxn(
          thisAcc.addr, await getTxnParams(),
          algosdk.OnApplicationComplete.NoOpOC,
          appApproval0_bin.result,
          appClear_bin.result,
          0, 0, 2, 1));

    const ApplicationID = createRes["application-index"];
    if ( ! ApplicationID ) {
      throw Error(`No application-index in ${JSON.stringify(createRes)}`);
    }
    const bin_comp = await compileFor(bin, ApplicationID);

    const params = await getTxnParams();
    const txnUpdate =
      algosdk.makeApplicationUpdateTxn(
        thisAcc.addr, params,
        ApplicationID, bin_comp.appApproval.result,
        appClear_bin.result);
    const txnToContract =
      algosdk.makePaymentTxnWithSuggestedParams(
        thisAcc.addr,
        bin_comp.ctc.hash,
        raw_minimumBalance,
        undefined, ui8z,
        params);
    const txnToHandlers: Array<Txn> =
      bin_comp.steps.flatMap((sc: CompileResultBytes|null): Array<Txn> => {
        if ( ! sc) { return []; }
        return [algosdk.makePaymentTxnWithSuggestedParams(
          thisAcc.addr,
          sc!.hash,
          raw_minimumBalance,
          undefined, ui8z,
          params)];
    });

    const txns = [
      txnUpdate,
      txnToContract,
      ...txnToHandlers
    ];
    algosdk.assignGroupID(txns);

    const txnUpdate_s =
      txnUpdate.signTxn(thisAcc.sk);
    const txnToContract_s =
      txnToContract.signTxn(thisAcc.sk);
    const txnToHandlers_s =
      txnToHandlers.map((tx: Txn): SignedTxn => tx.signTxn(thisAcc.sk));
    const txns_s = [
      txnUpdate_s,
      txnToContract_s,
      ...txnToHandlers_s,
    ];

    let updateRes;
    try {
        updateRes = await sendAndConfirm( txns_s, txnUpdate );
    } catch (e) {
      throw Error(`deploy: ${JSON.stringify(e)}`);
    }

    const creationRound = updateRes['confirmed-round'];
    const getInfo = async (): Promise<ContractInfo> =>
      ({ ApplicationID, creationRound });

    debug(`${shad} application created`);
    return await attachP(bin, getInfo());
  };

  /**
   * @description Push await down into the functions of a ContractAttached
   * @param implP A promise of an implementation of ContractAttached
   */
  const deferP = (implP: Promise<ContractAttached>): ContractAttached => {
    return {
      getInfo: async () => (await implP).getInfo(),
      sendrecv: async (...args: any) => (await implP).sendrecv(...args),
      recv: async (...args: any) => (await implP).recv(...args),
      wait: async(...args: any) => (await implP).wait(...args),
      iam, // doesn't need to await the implP
    }
  };

  const attach = (bin: Backend, ctcInfoP: Promise<ContractInfo>): ContractAttached => {
    return deferP(attachP(bin, ctcInfoP));
  };

  const deploy = (bin: Backend): ContractAttached => {
    return deferP(deployP(bin));
  };
  return { deploy, attach, networkAccount };
};

const getBalanceAt = async (addr: Address, round: Round): Promise<number> => {
  void(round);
  // XXX use indexer LookupAccountById(addr).round(round)
  return (await (await getAlgodClient()).accountInformation(addr).do()).amount;
};

export const balanceOf = async (acc: Account): Promise<BigNumber> => {
  const { networkAccount } = acc;
  if (!networkAccount) throw Error(`acc.networkAccount missing. Got: ${acc}`);
  return bigNumberify(await getBalanceAt(networkAccount.addr, await getLastRound()));
};

const showBalance = async (note: string, networkAccount: NetworkAccount) => {
  const bal = await balanceOf({ networkAccount });
  const showBal = formatCurrency(bal, 2);
  console.log('%s: balance: %s algos', note, showBal);
};

export const newTestAccount = async (startingBalance: BigNumber) => {
  const networkAccount = algosdk.generateAccount();
  if (getDEBUG()) { await showBalance('before', networkAccount); }
  await transfer({networkAccount: FAUCET}, {networkAccount}, startingBalance);
  if (getDEBUG()) { await showBalance('after', networkAccount); }
  return await connectAccount(networkAccount);
};

/** @description the display name of the standard unit of currency for the network */
export const standardUnit = 'ALGO';
/** @description the display name of the atomic (smallest) unit of currency for the network */
export const atomicUnit = 'μALGO';

/**
 * @description  Parse currency by network
 * @param amt  value in the {@link standardUnit} for the network.
 * @returns  the amount in the {@link atomicUnit} of the network.
 * @example  parseCurrency(100).toString() // => '100000000'
 */
export function parseCurrency(amt: CurrencyAmount): BigNumber {
  const numericAmt =
    isBigNumber(amt) ? amt.toNumber()
    : typeof amt === 'string' ? parseFloat(amt)
    : amt;
  return bigNumberify(algosdk.algosToMicroalgos(numericAmt));
}
// XXX get from SDK
const raw_minimumBalance = 100000;
export const minimumBalance: BigNumber =
  bigNumberify(raw_minimumBalance);

/**
 * @description  Format currency by network
 * @param amt  the amount in the {@link atomicUnit} of the network.
 * @param decimals  up to how many decimal places to display in the {@link standardUnit}.
 *   Trailing zeroes will be omitted. Excess decimal places will be truncated. (not rounded)
 *   This argument defaults to maximum precision.
 * @returns  a string representation of that amount in the {@link standardUnit} for that network.
 * @example  formatCurrency(bigNumberify('100000000')); // => '100'
 */
export function formatCurrency(amt: BigNumber, decimals: number = 6): string {
  // Recall that 1 algo = 10^6 microalgos
  if (!(Number.isInteger(decimals) && 0 <= decimals)) {
    throw Error(`Expected decimals to be a nonnegative integer, but got ${decimals}.`);
  }
  // Use decimals+1 and then slice it off to truncate instead of round
  const algosStr = algosdk.microalgosToAlgos(amt.toNumber()).toFixed(decimals+1);
  // Have to roundtrip thru Number to drop trailing zeroes
  return Number(algosStr.slice(0, algosStr.length - 1)).toString();
}

export async function getFaucet(): Promise<Account> {
  return await connectAccount(FAUCET);
}

// TODO: get from AlgoSigner if in browser
export async function getDefaultAccount(): Promise<Account> {
  return await getFaucet();
}

export const setFaucet = false; // XXX
export const newAccountFromMnemonic = false; // XXX
export const getNetworkTime = async () => bigNumberify(await getLastRound());
export const waitUntilTime = async (targetTime: BigNumber, onProgress?: OnProgress): Promise<BigNumber> => {
  const onProg = onProgress || (() => {});
  let currentTime = await getNetworkTime();
  while (currentTime.lt(targetTime)) {
    debug(`waitUntilTime: iteration: ${currentTime} -> ${targetTime}`);
    const status = await (await getAlgodClient()).statusAfterBlock(currentTime.toNumber()).do();
    currentTime = bigNumberify(status['last-round']);
    onProg({currentTime, targetTime});
  }
  debug(`waitUntilTime: ended: ${currentTime} -> ${targetTime}`);
  return currentTime;
};
export const wait = async (delta: BigNumber, onProgress?: OnProgress): Promise<BigNumber> => {
  const now = await getNetworkTime();
  debug(`wait: delta=${delta} now=${now}, until=${now.add(delta)}`);
  return await waitUntilTime(now.add(delta), onProgress);
};
export const verifyContract = false; // XXX
export const addressEq = mkAddressEq(T_Address);
