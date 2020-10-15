import crypto from 'crypto';
import ethers from 'ethers';

type BigNumber = ethers.BigNumber;
type num = BigNumber | number

const BigNumber = ethers.BigNumber;

export type OnProgress = (obj: {currentTime: BigNumber, targetTime: BigNumber}) => any;

export type IRecvNoTimeout<RawAddress> =  {
  didTimeout: false,
  data: Array<any>,
  value: BigNumber,
  from: RawAddress,
};

export type IRecv<RawAddress> = IRecvNoTimeout<RawAddress> | {
  didTimeout: true
}

export type IContract<ContractInfo, Digest, RawAddress> = {
  getInfo: () => Promise<ContractInfo>,
  sendrecv: (
    label: string, funcNum: number, evt_cnt: number, tys: Array<TyContract<any>>,
    args: Array<any>, value: BigNumber, out_tys: Array<TyContract<any>>,
    timeout_delay: BigNumber | false, sim_p: (fake: IRecv<RawAddress>) => ISimRes<Digest, RawAddress>,
  ) => Promise<IRecv<RawAddress>>,
  recv: (
    label: string, okNum: number, ok_cnt: number, out_tys: Array<TyContract<any>>,
    timeout_delay: BigNumber | false,
  ) => Promise<IRecv<RawAddress>>,
  wait: (delta: BigNumber) => Promise<BigNumber>,
  iam: (some_addr: RawAddress) => RawAddress,
};

export type IAccount<NetworkAccount, Backend, Contract, ContractInfo> = {
  networkAccount: NetworkAccount,
  deploy: (bin: Backend) => Contract,
  attach: (bin: Backend, ctc: ContractInfo | Promise<ContractInfo>) => Contract,
}

export type IAccountTransferable<NetworkAccount> = IAccount<NetworkAccount, any, any, any> | {
  networkAccount: NetworkAccount,
}

export type ISimRes<Digest, RawAddress> = {
  prevSt: Digest,
  txns: Array<ISimTxn<RawAddress>>,
  nextSt: Digest,
  isHalt : boolean,
};
export type ISimTxn<RawAddress> = {
  to: RawAddress,
  amt: BigNumber,
};

let DEBUG: boolean = process.env.REACH_DEBUG ? true : false;
export const setDEBUG = (b: boolean) => {
  if (b === false || b === true) {
    DEBUG = b;
  } else {
    throw Error(`Expected bool, got ${JSON.stringify(b)}`);
  }
};
export const getDEBUG = (): boolean => { return DEBUG; };
export const debug = (msg: any) => {
  if (getDEBUG()) {
    console.log(`[${(new Date()).toISOString()}] DEBUG: ${msg}`);
  }
};

export const assert = (d: any, ai: any = null) => {
  if (!d) {
    throw Error(format_ai(ai));
  }
}

const {
  hexlify,
  toUtf8Bytes,
  toUtf8String,
  isHexString,
} = ethers.utils;
export const { isBigNumber } = BigNumber;
export const bigNumberify = (x: any): BigNumber => BigNumber.from(x);

export const checkedBigNumberify = ( at:string, m:BigNumber, x:any ): BigNumber => {
  const xb = bigNumberify(x);
  if (xb.gte(0) && xb.lte(m)) {
    return xb;
  }
  throw Error(`bigNumberify: ${x} out of range [0, ${m}] at ${at}`);
};

// Hex helpers
// const un0x           = h => h.replace(/^0x/, ''); // unused
const hexTo0x = (h: string): string => '0x' + h.replace(/^0x/, '');
const byteToHex = (b: number): string => (b & 0xFF).toString(16).padStart(2, '0');
const byteArrayToHex = (b: any): string => Array.from(b, byteToHex).join('');

// const hexOf = x =>
//       typeof x === 'string' && x.slice(0, 2) === '0x'
//       ? un0x(toHex(x))
//       : un0x(toHex(`0x${x}`));
const hexOf = (x: any): string => toHex(x);
// TODO: why was this stripping off the 0x?
// Why was it slapping 0x on non-hex strings?


// Contracts

// .name is used for error display purposes only
// .canonicalize turns stuff into the "canonical backend representation"
// .munge expects a canonicalized value, and "munges" it for sending to the network
// .unmunge is the inverse of .munge
// TODO: decouple .munge and .unmunge from this module

export type TyContract<T> = {
  name: string,
  canonicalize: (v: any) => T,
  munge: (v: T) => any,
  unmunge: (v: any) => T,
  defaultValue: T,
};

export const T_Null: TyContract<null> = {
  name: 'Null',
  canonicalize: (v: any): null => {
    // Doesn't check with triple eq; we're being lenient here
    if (v != null) {
      throw Error(`Expected null, but got ${JSON.stringify(v)}`);
    }
    return null;
  },
  // null is represented in solidity as false
  munge: (v: null): false => { void(v); return false; },
  unmunge: (v: false): null => { void(v); return null; },
  defaultValue: null,
};

export const T_Bool: TyContract<boolean> = {
  name: 'Bool',
  canonicalize: (v: any): boolean => {
    if (typeof(v) !== 'boolean') {
      throw Error(`Expected boolean, but got ${JSON.stringify(v)}`);
    }
    return v;
  },
  munge: (v: boolean): boolean => v,
  unmunge: (v: boolean): boolean => v,
  defaultValue: false,
};

export const T_UInt: TyContract<BigNumber> = {
  name: 'UInt',
  canonicalize: (v: any): BigNumber => {
    if (isBigNumber(v)) {
      return v;
    }
    if (typeof(v) === 'number') {
      return bigNumberify(v);
    }
    if (typeof(v) === 'string') {
      if (v.slice(0, 2) == '0x' && v.length == 66) {
        // TODO: also check it is entirely 0-9 a-f
        return bigNumberify(v);
      } else {
        throw Error(`String does not represent a BigNumber. ${JSON.stringify(v)}`);
      }
    }
    throw Error(`Expected BigNumber or number, but got ${JSON.stringify(v)}`);
  },
  munge: (v: BigNumber): BigNumber => v,
  // TODO: double check:
  // It looks like munging BigNumber to string is no longer needed?
  // munge: (v) => v.toString(),
  unmunge: (v: BigNumber): BigNumber => v,
  defaultValue: bigNumberify(0),
};

// TODO: define some wrapper type Bytes?
export const T_Bytes: TyContract<string> = {
  name: 'Bytes',
  canonicalize: (x: any): string => {
    if (typeof(x) !== 'string') {
      throw Error(`Bytes expected string, but got ${JSON.stringify(x)}`);
    }
    if (isHex(x)) {
      return x;
    } else {
      return toHex(x);
      // TODO: fix things so this restriction is not necessary
      // throw Error(`Please use toHex on string sent to Reach: "${x}"`);
    }
  },
  munge: (v: string): string => v,
  unmunge: (v: string): string => v,
  defaultValue: '0x0',
};

export const T_Digest: TyContract<BigNumber> =
  Object.assign({}, T_UInt, { name: 'Digest' });

// TODO: use a wrapper type for canonicalized form
export const T_Address: TyContract<string> = {
  name: 'Address',
  canonicalize: (x: any): string => {
    if (typeof x !== 'string') {
      throw Error(`Address must be a string, but got: ${JSON.stringify(x)}`);
    }
    if (x.slice(0, 2) !== '0x') {
      throw Error(`Address must start with 0x, but got: ${JSON.stringify(x)}`);
    }
    if (!isHex(x)) {
      throw Error(`Address must be a valid hex string, but got: ${JSON.stringify(x)}`);
    }
    // TODO check address length?
    return x;
  },
  munge: (v: string): string => v,
  unmunge: (v: string): string => v,
  defaultValue: '0x' + Array(64).fill('0').join(''),
};

export const T_Array = <T>(ctc: TyContract <T> , sz: number): TyContract<Array<T>> => {
  // TODO: check ctc, sz for sanity
  return {
    name: `Array(${ctc.name}, ${sz})`,
    canonicalize: (args: any): Array<T> => {
      if (!Array.isArray(args)) {
        throw Error(`Expected an Array, but got ${JSON.stringify(args)}`);
      }
      if (sz != args.length) {
        throw Error(`Expected array of length ${sz}, but got ${args.length}`);
      }
      return args.map((arg) => ctc.canonicalize(arg));
    },
    munge: (v: Array<T>): Array<any> => {
      return v.map((arg) => ctc.munge(arg));
    },
    unmunge: (v: Array<any>): Array<T> => {
      return v.map((arg) => ctc.unmunge(arg));
    },
    defaultValue: (() => {
      return Array(sz).fill(ctc.defaultValue);
    })(),
  };
};

// TODO: way too hard to figure out how to teach typescript the type of this
// T is just the "union of all types in the tuple"
export const T_Tuple = <T>(ctcs: Array <TyContract<T>>): TyContract<Array<T>> => {
  // TODO: check ctcs for sanity
  return {
    name: `Tuple(${ctcs.map((ctc) => ` ${ctc.name} `)})`,
    canonicalize: (args: any): Array<T> => {
      if (!Array.isArray(args)) {
        throw Error(`Expected a Tuple, but got ${JSON.stringify(args)}`);
      }
      if (ctcs.length != args.length) {
        throw Error(`Expected tuple of size ${ctcs.length}, but got ${args.length}`);
      }
      return args.map((arg, i) => ctcs[i].canonicalize(arg));
    },
    munge: (args: Array<T>): Array<any> => {
      return args.map((arg, i) => ctcs[i].munge(arg));
    },
    unmunge: (args: Array<any>): Array<T> => {
      return args.map((arg: any, i: number) => ctcs[i].unmunge(arg));
    },
    defaultValue: (() => {
      return ctcs.map(ctc => ctc.defaultValue);
    })(),
  };
};


// TODO: way too hard to teach typescript the type of this
// T is just the "union of all object value types"
export const T_Object = <T>(co: {
  [key: string]: TyContract <T>
}): TyContract <{[key: string]: T}> => {
  // TODO: check co for sanity
  return {
    name: `Object(${Object.keys(co).map((k) => ` ${k}: ${co[k].name} `)})`,
    canonicalize: (vo: any): {
      [key: string]: T
    } => {
      if (typeof(vo) !== 'object') {
        throw Error(`Expected object, but got ${JSON.stringify(vo)}`);
      }
      const obj: {
        [key: string]: T
      } = {};
      for (const prop in co) {
        // This is dumb but it's how ESLint says to do it
        // https://eslint.org/docs/rules/no-prototype-builtins
        if (!{}.hasOwnProperty.call(vo, prop)) {
          throw Error(`Expected prop ${prop}, but didn't found it in ${Object.keys(vo)}`);
        }
        obj[prop] = co[prop].canonicalize(vo[prop]);
      }
      return obj;
    },
    munge: (vo: {
      [key: string]: T
    }): {
      [key: string]: any
    } => {
      const obj: {
        [key: string]: any
      } = {};
      for (const prop in co) {
        obj[prop] = co[prop].munge(vo[prop]);
      }
      return obj;
    },
    // TODO: reduce duplication somehow
    unmunge: (vo: {
      [key: string]: any
    }): {
      [key: string]: T
    } => {
      const obj: {
        [key: string]: T
      } = {};
      for (const prop in co) {
        obj[prop] = co[prop].unmunge(vo[prop]);
      }
      return obj;
    },
    defaultValue: (() => {
      const obj: {
        [key: string]: T
      } = {};
      for (const prop in co) {
        obj[prop] = co[prop].defaultValue;
      }
      return obj;
    })(),
  };
};

// TODO: way too hard to teach typescript the type of this
// T is just the "union of all variant types"
export const T_Data = <T>(co: {
  [key: string]: TyContract<T>
}): TyContract<[string, T]> => {
  // TODO: check co for sanity
  // ascLabels[i] = label
  // labelMap[label] = i
  const ascLabels = Object.keys(co).sort();
  const labelMap: {
    [key: string]: number
  } = {};
  for (const i in ascLabels) {
    labelMap[ascLabels[i]] = parseInt(i);
  }
  return {
    name: `Data(${Object.keys(co).map((k) => ` ${k}: ${co[k].name} `)})`,
    canonicalize: (io: any): [string, T] => {
      if (!(Array.isArray(io) && io.length == 2)) {
        throw Error(`Expected an array of length two to represent a data instance, but got ${JSON.stringify(io)}`);
      }
      const vn = io[0];
      if (!{}.hasOwnProperty.call(co, vn)) {
        throw Error(`Expected a variant in ${Object.keys(co)}, but got ${vn}`);
      }
      return [vn, co[vn].canonicalize(io[1])];
    },
    // Data representation in js is a 2-tuple:
    // [label, val]
    // where label : string
    // and val : co[label]
    //
    // Data representation in solidity is an N+1-tuple: (actually a struct)
    // [labelInt, v0, ..., vN]
    // where labelInt : number, 0 <= labelInt < N
    // vN : co[ascLabels[i]]
    //
    munge: (vt: [string, T]): Array<any> => {
      // Typescript is stupid about destructuring tuple tupes =(
      const label = vt[0];
      const v = vt[1];
      const i = labelMap[label];
      const vals = ascLabels.map((label) => {
        const vco = co[label];
        return vco.munge(vco.defaultValue);
      });
      vals[i] = co[label].munge(v);
      return [i].concat(vals);
    },
    // Note: when it comes back from solidity, vs behaves like an N+1-tuple,
    // but also has secret extra keys you can access,
    // based on the struct field names.
    // e.g. Maybe has keys vs["which"], vs["_None"], and vs["_Some"],
    // corresponding to    vs[0],       vs[1],       and vs[2] respectively.
    // We don't currently use these, but we could.
    unmunge: (vs: Array<any>): [string, T] => {
      const i = vs[0];
      const label = ascLabels[i];
      const val = vs[i + 1];
      return [label, co[label].unmunge(val)];
    },
    defaultValue: ((): [string, T] => {
      const label = ascLabels[0];
      return [label, co[label].defaultValue];
    })(),
  };
};

const format_ai = (ai: any) => JSON.stringify(ai);

export function protect <T>(ctc: TyContract<T>, v: any, ai: any = null) {
  try {
    return ctc.canonicalize(v);
  } catch (e) {
    console.log(`Protect failed: expected ${ctc.name} but got ${JSON.stringify(v)}${format_ai(ai)}`);
    throw e;
  }
}

// Massage the arg into a form keccak256 will handle correctly
let digestWidth = 32;
export const setDigestWidth = (sz: number): void => {
  digestWidth = sz;
};
const kek = (arg: any): string | Uint8Array => {
  if (typeof(arg) === 'string') {
    if (isHex(arg)) {
      return arg;
    } else {
      return toUtf8Bytes(arg);
    }
  } else if (typeof(arg) === 'number') {
    return '0x' + bigNumberToHex(arg, digestWidth);
  } else if (isBigNumber(arg)) {
    return '0x' + bigNumberToHex(arg, digestWidth);
  } else if (arg && arg.constructor && arg.constructor.name == 'Uint8Array') {
    return arg;
  } else if (Array.isArray(arg)) {
    return ethers.utils.concat(arg.map((x) => ethers.utils.arrayify(kek(x))));
  } else {
    throw Error(`Can't kek this: ${arg}`);
  }
};

export const toHex = (x: any) => hexlify(kek(x));
export const isHex = isHexString;
export const hexToString = toUtf8String;

// XXX the JS backend expects this to be a BigNumber
export const digest = (...args: Array<any>) => {
  debug(`digest(${JSON.stringify(args)}) =>`);
  const kekCat = kek(args);
  debug(`digest(${JSON.stringify(args)}) => ${JSON.stringify(kekCat)}`);
  const r = ethers.utils.keccak256(kekCat);
  debug(`keccak(${JSON.stringify(args)}) => internal(${JSON.stringify(kekCat)}) => ${JSON.stringify(r)}`);
  return r;
};

export const hexToBigNumber = (h: string): BigNumber => bigNumberify(hexTo0x(h));
export const uintToBytes = (i: BigNumber): string => bigNumberToHex(i);

export const bigNumberToHex = (u: num, size: number = 32) => {
  const width = 8 * size;
  const format = `ufixed${width}x0`;
  const nPos = bigNumberify(u).toTwos(width);
  // They took away padZeros so we have to use FixedNumber
  const nFix = ethers.FixedNumber.from(nPos.toString(), format);
  // XXX why do we slice off the 0x?
  return hexlify(nFix).slice(2);
};

export const bytesEq = (x: any, y: any): boolean =>
  hexOf(x) === hexOf(y);
export const digestEq = bytesEq;
export const addressEq = bytesEq;

export const randomUInt = (): BigNumber =>
  hexToBigNumber(byteArrayToHex(crypto.randomBytes(digestWidth)));

export const hasRandom = {
  random: randomUInt,
};

export const eq = (a: num, b: num): boolean => bigNumberify(a).eq(bigNumberify(b));
export const add = (a: num, b: num): BigNumber => bigNumberify(a).add(bigNumberify(b));
export const sub = (a: num, b: num): BigNumber => bigNumberify(a).sub(bigNumberify(b));
export const mod = (a: num, b: num): BigNumber => bigNumberify(a).mod(bigNumberify(b));
export const mul = (a: num, b: num): BigNumber => bigNumberify(a).mul(bigNumberify(b));
export const div = (a: num, b: num): BigNumber => bigNumberify(a).div(bigNumberify(b));
export const ge = (a: num, b: num): boolean => bigNumberify(a).gte(bigNumberify(b));
export const gt = (a: num, b: num): boolean => bigNumberify(a).gt(bigNumberify(b));
export const le = (a: num, b: num): boolean => bigNumberify(a).lte(bigNumberify(b));
export const lt = (a: num, b: num): boolean => bigNumberify(a).lt(bigNumberify(b));

// Array helpers

export function Array_set <T>(arr: Array<T>, idx: number, elem: T): Array<T> {
  const arrp = arr.slice();
  arrp[idx] = elem;
  return arrp;
}

export const Array_zip = <X,Y>(x: Array<X>, y: Array<Y>): Array<[X, Y]> =>
  x.map((e, i): [X, Y] => [e, y[i]]);

export type CurrencyAmount = string | number | BigNumber
export type {Connector} from './ConnectorMode';
