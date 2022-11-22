// This has no dependencies on other shared things
import { ethers } from 'ethers';
import {
  bigNumberify,
  bigNumberToNumber,
} from './CBR';
import {
  apiStateMismatchError,
  debug,
  j2s,
  MapRefT,
  GetKeyT,
  IContractCompiledMaps,
} from './shared_impl';
export {
  bigNumberToNumber,
  apiStateMismatchError,
};
void(debug);

type BigNumber = ethers.BigNumber;
export type num = BigNumber | number

export type MaybeRep<A> = ['Some', A] | [ 'None', null ]
export const asMaybe = <A>(v:A|undefined): MaybeRep<A> => {
  if ( v === undefined ) {
    return ['None', null];
  } else {
    return ['Some', v];
  }
};

export const fromSome = <A>(mo:MaybeRep<A>, da:A): A => {
  if ( mo[0] === 'Some' ) {
    return mo[1];
  } else {
    return da;
  }
};

export interface AnyBackendTy {
  name: string,
  canonicalize: (x: any) => any,
};

const objectIsEmpty = (obj: any) =>
  (obj
  && Object.keys(obj).length === 0
  && Object.getPrototypeOf(obj) === Object.prototype);

type AssertInfo =
  unknown |
  undefined |
  string |
  {
    who?: string,
    msg?: string|null,
    at?: string,
    fs?: [string],
  };

function hasProp<K extends PropertyKey>(data: object, prop: K): data is Record<K, unknown> {
    return prop in data;
}
export const formatAssertInfo = (ai:AssertInfo) => {
  let msg = '';
  if ( typeof ai === 'string' ) {
    msg = `: ${ai}`;
  } else if ( ai === null || ai === undefined ) {
  } else if ( typeof ai === 'object' ) {
  if ( hasProp(ai, 'who') ) {
    msg += `: ${ai.who}`;
    delete ai.who;
  }
  if ( hasProp(ai, 'msg') ) {
    if ( ai.msg !== null ) {
      msg += `: ${ai.msg}`;
    }
    delete ai.msg;
  }
  if ( hasProp(ai, 'at') ) {
    msg += `\n  at ${ai.at}`;
    delete ai.at;
  }
  let rest = `:`;
  if ( hasProp(ai, 'fs') && Array.isArray(ai.fs) ) {
    for ( const f of ai.fs ) {
      msg += `\n  ${f}`;
    }
    delete ai.fs;
    rest = `\n`;
  }
  if ( ! objectIsEmpty(ai) ) {
    msg += `${rest} ${j2s(ai)}`;
  }
  }
  return msg;
};

export const assert = (d: any, ai: any = {}) => {
  if (!d) {
    throw Error(`Assertion failed${formatAssertInfo(ai)}`);
  }
};

export const checkedBigNumberify = ( at:string, m:BigNumber, x:any ): BigNumber => {
  const xb = bigNumberify(x);
  if (xb.gte(0) && xb.lte(m)) {
    return xb;
  }
  throw Error(`bigNumberify: ${x} out of range [0, ${m}] at ${at}`);
};

export function protect (ctc: AnyBackendTy, v: unknown, ai: unknown = null) {
  debug(`protect`, ctc.name, v);
  try {
    // .canonicalize turns stuff into the "canonical backend representation"
    return ctc.canonicalize(v);
  } catch (e) {
    throw Error(`Protect failed: expected ${ctc.name} but got ${j2s(v)}${formatAssertInfo(ai)}\n${j2s(e)}`);
  }
};

export function bytesFromHex(v:any) {
  return ethers.utils.arrayify(v);
}

const {
  toUtf8Bytes,
  isHexString,
} = ethers.utils;

export const hexlify = ethers.utils.hexlify;

export const isHex = isHexString;

export const stringToHex = (x:string): string =>
  hexlify(toUtf8Bytes(x));

const hexToInt = (x: string): number => parseInt(x, 16);

const forceHex = (x: string): string =>
  isHex(x) ? x : stringToHex(x);

export const bytesEq = (x: any, y: any): boolean => {
  return forceHex(x) === forceHex(y); };

export const bytesConcat = (x: string, y: string): string => {
  // forceHex(x).concat(forceHex(y).slice(2));
  return x.concat(y);
};

export const eq = (a: num, b: num): boolean => bigNumberify(a).eq(bigNumberify(b));
export const ge = (a: num, b: num): boolean => bigNumberify(a).gte(bigNumberify(b));
export const gt = (a: num, b: num): boolean => bigNumberify(a).gt(bigNumberify(b));
export const le = (a: num, b: num): boolean => bigNumberify(a).lte(bigNumberify(b));
export const lt = (a: num, b: num): boolean => bigNumberify(a).lt(bigNumberify(b));
export const eq256 = eq;
export const ge256 = ge;
export const gt256 = gt;
export const le256 = le;
export const lt256 = lt;

export const stringDynConcat = (s1: string, s2: string): string => `${s1}${s2}`;

export const uintToStringDyn = (n1: num): string => n1.toString();
export const uintToStringDyn256 = uintToStringDyn;

export const digest_xor = (xd: string, yd: string): string => {
  const clean = (s: string) => s.slice(0, 2) === '0x' ? s.slice(2) : s;
  const xc = clean(xd);
  const yc = clean(yd);

  const parseHex = (xs: string) => {
    const ret = [];
    for (let i = 0; i < xs.length; i += 2) {
      ret.push(hexToInt(xs.substring(i, i + 2)));
    }
    return ret;
  }

  const xs = parseHex(xc);
  const ys = parseHex(yc);

  const result = '0x' + xs.map((x: number, i: number) => (x ^ ys[i]).toString(16).padStart(2, '0')).join('');
  return result;
}
export const bytes_xor = (x: string, y: string): string => {
  const xs = Buffer.from(x);
  const ys = Buffer.from(y);

  const xors = xs.map((x, i) => x ^ ys[i]);
  return String.fromCharCode(...xors);
}

export const btoiLast8 = (b: string): BigNumber => {
  const min = (b.length < 8) ? 0 : b.length - 8;
  const bb = Buffer.from(b);
  let res = bigNumberify(0);
  for (let i = min; i < b.length; i++) {
    res = res.mul(256).add(bb[i]);
  }
  return res;
}

export function Array_set <T>(arr: Array<T>, idx: number, elem: T): Array<T> {
  const arrp = arr.slice();
  arrp[idx] = elem;
  return arrp;
}

export interface MapOpts<ConnectorTy extends AnyBackendTy> {
  ctc: IContractCompiledMaps<ConnectorTy>,
  isAPI: boolean,
  idx: number,
};
export interface LinearMap<K, A, ConnectorTy extends AnyBackendTy> {
  getKey: GetKeyT<K, ConnectorTy>,
  ref: MapRefT<K, A, ConnectorTy>,
  set: (kt:ConnectorTy, k:K, vt:ConnectorTy, v:A|undefined) => Promise<void>,
};

const basicMap = <K, A, ConnectorTy extends AnyBackendTy>(getKey:GetKeyT<K, ConnectorTy>): LinearMap<K, A, ConnectorTy> => {
  const m: {[key: string]: A|undefined} = {};
  const basicSet = async (kt:ConnectorTy, k:K, vt:ConnectorTy, v:A|undefined): Promise<void> => {
    const [f, mbr] = await getKey(kt, k, vt); void mbr;
    m[f] = v;
  };
  const basicRef = async (kt:ConnectorTy, k:K, vt:ConnectorTy): Promise<MaybeRep<A>> => {
    const [f, mbr] = await getKey(kt, k, vt); void mbr;
    return asMaybe<A>(m[f]);
  };
  return { getKey, ref: basicRef, set: basicSet };
};
export const copyMap = <K, A, ConnectorTy extends AnyBackendTy>(orig:LinearMap<K, A, ConnectorTy>): LinearMap<K, A, ConnectorTy> => {
  const { getKey, ref: origRef } = orig;
  const m: LinearMap<K, A, ConnectorTy> = basicMap(getKey);
  const seen: {[key: string]: boolean} = {};
  const copySet = async (kt:ConnectorTy, k:K, vt:ConnectorTy, v:A|undefined): Promise<void> => {
    const [f, mbr] = await getKey(kt, k, vt); void mbr;
    seen[f] = true;
    await mapSet(m, kt, k, vt, v);
  };
  const copyRef = async (kt:ConnectorTy, k:K, vt:ConnectorTy): Promise<MaybeRep<A>> => {
    const [f, mbr] = await getKey(kt, k, vt); void mbr;
    if ( ! seen[f] ) {
      const mv = await origRef(kt, k, vt);
      await copySet(kt, k, vt, mv[0] === 'Some' ? mv[1] : undefined);
    }
    return await mapRef(m, kt, k, vt);
  };
  return { getKey, ref: copyRef, set: copySet };
};

// dupe: () => {[key: string]: A},
export const newMap = <K, A, ConnectorTy extends AnyBackendTy>(opts: MapOpts<ConnectorTy>): LinearMap<K, A, ConnectorTy> => {
  const { makeGetKey, apiMapRef } = opts.ctc;
  const getKey = makeGetKey(opts.idx);
  if ( opts.isAPI ) {
    const fake: LinearMap<K, A, ConnectorTy> = {
      getKey,
      ref: apiMapRef(opts.idx),
      set: async (kt, k, vt, v) => { void kt; void k; void vt; void v; },
    }
    return copyMap(fake);
  } else {
    return basicMap(getKey);
  }
};
export const mapSet = async <K, A, Ty extends AnyBackendTy>(m: LinearMap<K, A, Ty>, kt:Ty, k:K, vt:Ty, v:A|undefined): Promise<void> => {
  await m.set(kt, k, vt, v);
};
export const mapRef = async <K, A, Ty extends AnyBackendTy>(m: LinearMap<K, A, Ty>, kt:Ty, k:K, vt:Ty): Promise<MaybeRep<A>> => {
  return await m.ref(kt, k, vt);
};

export const Array_asyncMap = async <B>(as:any[][], f:(x:any[], i:number) => Promise<B>): Promise<B[]> => {
  const fWrap = (_a:any, i:number) => f(as.map((a) => a[i]), i);
  return Promise.all(as[0].map(fWrap));
};

export const Array_asyncReduce = async <B>(as:any[][], b: B, f:((xs:any[], y:B, i:number) => Promise<B>)): Promise<B> => {
  let accum = b;
  let i = 0;
  for ( i = 0; i < as[0].length; i++) {
    const as_i = as.map((a) => a[i]);
    accum = await f(as_i, accum, i);
  }
  return accum;
};
