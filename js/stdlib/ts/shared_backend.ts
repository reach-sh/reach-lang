// This has no dependencies on other shared things
import { ethers } from 'ethers';
import {
  bigNumberify
} from './CBR';
import {
  debug,
  j2s,
} from './shared_impl';
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

const formatAssertInfo = (ai:any = null) => {
  let msg = '';
  if ( typeof ai === 'string' ) {
    msg = `: ${ai}`;
  } else {
  if ( ai.who ) {
    msg += `: ${ai.who}`;
    delete ai.who;
  }
  if ( ai.msg !== undefined ) {
    if ( ai.msg !== null ) {
      msg += `: ${ai.msg}`;
    }
    delete ai.msg;
  }
  if ( ai.at ) {
    msg += `\n  at ${ai.at}`;
    delete ai.at;
  }
  let rest = `:`;
  if ( Array.isArray(ai.fs) ) {
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

export const assert = (d: any, ai: any = null) => {
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

export const Array_asyncMap = <A, B>(a: A[], f:((x:A, i:number) => Promise<B>)): Promise<B[]> => Promise.all(a.map(f));

export const Array_asyncReduce = async <A, B>(a: A[], b:B, f:((y:B, x:A, i:number) => Promise<B>)): Promise<B> => {
  let y = b;
  let i = 0;
  for ( const x of a ) {
    y = await f(y, x, i++);
  }
  return y;
};

export const Array_zip = <X,Y>(x: Array<X>, y: Array<Y>): Array<[X, Y]> =>
  x.map((e, i): [X, Y] => [e, y[i]]);

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

export const simTokenNew = (sim_r:any, n:any, s:any, u:any, m:any, p:any, d:any): any => {
  sim_r.txns.push({kind: 'tokenNew', n, s, u, m, p, d });
  // XXX This 0 is a hack... on Algorand we can't know at simulation time what
  // this is going to be... so this will cause a runtime exception from
  // something if it gets looked at (i.e. if you try to create and immediately
  // use it)
  return 0;
};

export const simTokenBurn = (sim_r:any, tok:any, amt:any): void => {
  sim_r.txns.push({kind: 'tokenBurn', tok, amt});
};

export const simTokenDestroy = (sim_r:any, tok:any): void => {
  sim_r.txns.push({kind: 'tokenDestroy', tok});
};
