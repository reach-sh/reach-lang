// This has no dependencies on other shared things
import { ethers } from 'ethers';
import {
  bigNumberify,
  bigNumberToNumber,
} from './CBR';
import {
  debug,
  j2s,
} from './shared_impl';
export {
  bigNumberToNumber,
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
export const digest_xor = (xd: string, yd: string): string => {
  const clean = (s: string) => s.slice(0, 2) === '0x' ? s.slice(2) : s;
  const xc = clean(xd);
  const yc = clean(yd);

  const parseHex = (xs: string) => {
    const ret = [];
    for (let i = 0; i < xs.length; i += 2) {
      ret.push(hexToInt(xs.substr(i, 2)));
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

export type MapRefT<A> = (f:string) => Promise<MaybeRep<A>>;
export interface MapOpts<A> {
  ctc: {
    apiMapRef: (i:number, ty: unknown) => MapRefT<A>
  },
  ty: unknown,
  isAPI: boolean,
  idx: number,
};
export interface LinearMap<A> {
  ref: MapRefT<A>,
  set: (f:string, v:A|undefined) => Promise<void>,
};

const basicMap = <A>(): LinearMap<A> => {
  const m: {[key: string]: A|undefined} = {};
  const basicSet = async (f:string, v:A|undefined): Promise<void> => {
    m[f] = v;
  };
  const basicRef = async (f:string): Promise<MaybeRep<A>> => {
    return asMaybe<A>(m[f]);
  };
  return { ref: basicRef, set: basicSet };
};
const copyMap = <A>(or: MapRefT<A>): LinearMap<A> => {
  const m: LinearMap<A> = basicMap();
  const seen: {[key: string]: boolean} = {};
  const copySet = async (f:string, v:A|undefined): Promise<void> => {
    seen[f] = true;
    await mapSet(m, f, v);
  };
  const copyRef = async (f:string): Promise<MaybeRep<A>> => {
    if ( ! seen[f] ) {
      const mv = await or(f);
      await copySet(f, mv[0] === 'Some' ? mv[1] : undefined);
    }
    return await mapRef(m, f);
  };
  return { ref: copyRef, set: copySet };
};

// dupe: () => {[key: string]: A},
export const newMap = <A>(opts: MapOpts<A>): LinearMap<A> => {
  if ( opts.isAPI ) {
    return copyMap(opts.ctc.apiMapRef(opts.idx, opts.ty));
  } else {
    return basicMap();
  }
};
export const mapSet = async <A>(m: LinearMap<A>, f: string, v: A|undefined): Promise<void> => {
  await m.set(f, v);
};
export const mapRef = async <A>(m: LinearMap<A>, f: string): Promise<MaybeRep<A>> => {
  return await m.ref(f);
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

export const simMapDupe = <A>(sim_r:any, mapi:number, mapo:LinearMap<A>): void => {
  sim_r.maps[mapi] = copyMap(mapo.ref);
};

const simMapLog = (sim_r:any, f: string): void => {
  sim_r.mapRefs.push(f);
};

export const simMapRef = async <A>(sim_r:any, mapi:number, f: string): Promise<MaybeRep<A>> => {
  simMapLog(sim_r, f);
  return await mapRef(sim_r.maps[mapi], f);
};

export const simMapSet = async <A>(sim_r:any, mapi:number, f: string, nv: A): Promise<void> => {
  simMapLog(sim_r, f);
  return await mapSet(sim_r.maps[mapi], f, nv);
};

export const simTokenNew = (sim_r:any, n:any, s:any, u:any, m:any, p:any, d:any, ctr:any): any => {
  sim_r.txns.push({kind: 'tokenNew', n, s, u, m, p, d });
  // XXX This is a hack... it is assumed that `ctr` is unique across tokens in a simulation block
  return ctr;
};

export const simTokenBurn = (sim_r:any, tok:any, amt:any): void => {
  sim_r.txns.push({kind: 'tokenBurn', tok, amt});
};

export const simTokenDestroy = (sim_r:any, tok:any): void => {
  sim_r.txns.push({kind: 'tokenDestroy', tok});
};
