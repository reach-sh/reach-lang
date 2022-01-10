import { ethers } from 'ethers';
import * as shared_backend from './shared_backend';
import * as CBR from './CBR';
const { bigNumberify, bigNumberToNumber } = CBR;

import type { // =>
  BigNumber
} from 'ethers';
import type { // =>
  CBR_Address,
  CBR_Array,
  CBR_Bool,
  CBR_Bytes,
  CBR_Data,
  CBR_Digest,
  CBR_Null,
  CBR_Object,
  CBR_Struct,
  CBR_Tuple,
  CBR_UInt,
  CBR_Val,
} from './CBR';
import type { // =>
  ETH_Ty,
  AnyETH_Ty,
  // EthLikeCompiled, // TODO: use once types are in place
  EthLikeCompiledArgs,
} from './ETH_like_interfaces'
import {
  Arith,
  TypeDefs,
  Stdlib_Backend_Base
} from './interfaces';
import {
  labelMaps,
  MkPayAmt,
  makeDigest,
  hexToString,
  mkAddressEq,
  makeArith,
} from './shared_impl';
export type { // =>
  ETH_Ty,
  AnyETH_Ty,
}

// Types
export type Token = CBR_Address;
export type PayAmt = MkPayAmt<Token>;

// TODO: restore return type annotation once types are in place
export function makeEthLikeCompiled(ethLikeCompiledArgs: EthLikeCompiledArgs) {
// ...............................................
const {
  T_Address,
} = ethLikeCompiledArgs;

const UInt_max: BigNumber =
  ethers.BigNumber.from(2).pow(256).sub(1);

const digest = makeDigest('keccak256', (t:AnyETH_Ty, v:any) => {
  // Note: abiCoder.encode doesn't correctly handle an empty tuple type
  if (t.paramType === 'tuple()') {
    if (Array.isArray(v) && v.length === 0) {
      return v;
    } else {
      throw Error(`impossible: digest tuple() with non-empty array: ${JSON.stringify(v)}`);
    }
  }
  return ethers.utils.defaultAbiCoder.encode([t.paramType], [t.munge(v)])
});

const V_Null: CBR_Null = null;

// null is represented in solidity as true
const T_Null: ETH_Ty<CBR_Null, true> = {
  ...CBR.BT_Null,
  defaultValue: V_Null,
  munge: (bv: CBR_Null): true => (void(bv), true),
  unmunge: (nv: true): CBR_Null => (void(nv), V_Null),
  paramType: 'bool',
};

const T_Bool: ETH_Ty<CBR_Bool, boolean> = {
  ...CBR.BT_Bool,
  defaultValue: false,
  munge: (bv: CBR_Bool): boolean => bv,
  unmunge: (nv: boolean): CBR_Bool => V_Bool(nv),
  paramType: 'bool',
};

const V_Bool = (b: boolean): CBR_Bool => {
  return T_Bool.canonicalize(b);
};

const T_UInt: ETH_Ty<CBR_UInt, BigNumber> = {
  ...CBR.BT_UInt(UInt_max),
  defaultValue: ethers.BigNumber.from(0),
  munge: (bv: CBR_UInt): BigNumber => bigNumberify(bv),
  unmunge: (nv: BigNumber): CBR_UInt => V_UInt(nv),
  paramType: 'uint256',
};

const V_UInt = (n: BigNumber): CBR_UInt => {
  return T_UInt.canonicalize(n);
};

// XXX figure out how to move this into cfxers instead of here?
// Conflux seems to sometimes turn uint8array into an array of bigint.
// This is silly and needs to be undone or else hexlify will die.
function unBigInt<T>(x: T): number[]|T {
  if (Array.isArray(x)) {
    return x.map((n: any): number => {
      if (typeof n === 'bigint') {
        if (n >= 256) throw Error(`unBigInt expected n < 256`);
        return Number(n);
      }
      return n;
    });
  } else {
    return x;
  }
}

function splitToChunks<T>(arr: T[], chunkSize: number): T[][] {
  const cs: T[][] = [];
  for (let i = 0; i < Math.ceil(arr.length / chunkSize); i++) {
    cs.push(arr.slice(i * chunkSize, (i + 1) * chunkSize));
  }
  return cs;
};

type ETH_Bytes = Array<Array<number>>;
const T_Bytes = (len:number): ETH_Ty<CBR_Bytes, ETH_Bytes> => {
  const me = {
    ...CBR.BT_Bytes(len),
    defaultValue: ''.padEnd(len, '\0'),
    munge: ((bv: CBR_Bytes): ETH_Bytes => {
      return splitToChunks(Array.from(ethers.utils.toUtf8Bytes(bv)), 32);
    }),
    unmunge: ((nvs: ETH_Bytes): CBR_Bytes => {
      const nvs_s = nvs.map((nv:any): any => hexToString(ethers.utils.hexlify(unBigInt(nv))));
      const nvss = "".concat(...nvs_s);
      // debug(me.name, nvs, nvss);
      return me.canonicalize(nvss);
    }),
    paramType: (() => {
      let n = len;
      const fs = [];
      while ( 0 < n ) {
        const ell = Math.min(32, n);
        fs.push(`bytes${ell}`);
        n = n - ell;
      }
      return `tuple(${fs.join(',')})`;
    })(),
  };
  return me;
};

const T_Digest: ETH_Ty<CBR_Digest, BigNumber> = {
  ...CBR.BT_Digest,
  defaultValue: ethers.utils.keccak256([]),
  munge: (bv: CBR_Digest): BigNumber => ethers.BigNumber.from(bv),
  // XXX likely not the correct unmunge type?
  unmunge: (nv: BigNumber): CBR_Digest => V_Digest(nv.toHexString()),
  paramType: 'uint256',
};
const V_Digest = (s: string): CBR_Digest => {
  return T_Digest.canonicalize(s);
};

const T_Array = <T>(
  ctc: ETH_Ty<CBR_Val, T>,
  size: number,
): ETH_Ty<CBR_Array, Array<T>> => ({
  ...CBR.BT_Array(ctc, size),
  defaultValue: Array(size).fill(ctc.defaultValue),
  munge: (bv: CBR_Array): any => {
    if ( size == 0 ) {
      return false;
    } else {
      return bv.map((arg: CBR_Val) => ctc.munge(arg));
    }
  },
  unmunge: (nv: Array<T>): CBR_Array => {
    if ( size == 0 ) {
      return [];
    } else {
      return V_Array(ctc, size)(nv.map((arg: T) => ctc.unmunge(arg)));
    }
  },
  paramType: `${ctc.paramType}[${size}]`,
});

const V_Array = <T>(
  ctc: ETH_Ty<CBR_Val, T>,
  size: number,
) => (val: Array<unknown>): CBR_Array => {
  return T_Array(ctc, size).canonicalize(val);
};

// XXX fix me Dan, I'm type checking wrong!
const T_Tuple = <T>(
  ctcs: Array<ETH_Ty<CBR_Val, T>>,
): ETH_Ty<CBR_Tuple, Array<T>> => ({
  ...CBR.BT_Tuple(ctcs),
  defaultValue: ctcs.map(ctc => ctc.defaultValue),
  munge: (bv: CBR_Tuple): any => {
    if (ctcs.length == 0 ) {
      return false;
    } else {
      return bv.map((arg, i) => ctcs[i].munge(arg));
    }
  },
  unmunge: (args: Array<T>): CBR_Tuple => {
    return V_Tuple(ctcs)(ctcs.map((ctc: any, i: number) => ctc.unmunge(args[i])));
  },
  paramType: `tuple(${ctcs.map((ctc) => ctc.paramType).join(',')})`
});

const V_Tuple = <T>(
  ctcs: Array<ETH_Ty<CBR_Val, T>>,
) => (val: Array<unknown>) => {
  return T_Tuple(ctcs).canonicalize(val);
}

const T_Struct = <T>(
  ctcs: Array<[string, ETH_Ty<CBR_Val, T>]>,
): ETH_Ty<CBR_Struct, Array<T>> => ({
  ...CBR.BT_Struct(ctcs),
  defaultValue: (() => {
    const obj: {[key: string]: CBR_Val} = {};
    ctcs.forEach(([prop, co]) => {
      obj[prop] = co.defaultValue;
    });
    return obj;
  })(),
  munge: (bv: CBR_Struct): any => {
    if (ctcs.length == 0 ) {
      return false;
    } else {
      return ctcs.map(([k, ctc]) => ctc.munge(bv[k]));
    }
  },
  unmunge: (args: any): CBR_Struct => {
    return V_Struct(ctcs)(ctcs.map(([k, ctc], i: number) => { void (k); return ctc.unmunge(args[i]); }));
  },
  paramType: `tuple(${ctcs.map(([k, ctc]) => { void (k); return ctc.paramType }).join(',')})`
});

const V_Struct = <T>(
  ctcs: Array<[string, ETH_Ty<CBR_Val, T>]>,
) => (val: any) => {
  return T_Struct(ctcs).canonicalize(val);
}

const T_Object = <T>(
  co: {[key: string]: ETH_Ty<CBR_Val, T>}
): ETH_Ty<CBR_Object, {[key: string]: T}> => ({
  ...CBR.BT_Object(co),
  defaultValue: (() => {
    const obj: {[key: string]: CBR_Val} = {};
    for (const prop in co) {
      obj[prop] = co[prop].defaultValue;
    }
    return obj;
  })(),
  // CBR -> Net . ETH object fields are prefaced with "_"
  munge: (bv: CBR_Object): any => {
    const obj: {
      [key: string]: any
    } = {};
    let none: boolean = true;
    for (const prop in co) {
      none = false;
      obj["_" + prop] = co[prop].munge(bv[prop]);
    }
    if ( none ) {
      return false;
    } else {
      return obj;
    }
  },
  unmunge: (bv: {[key: string]: T}): CBR_Object => {
    const obj: {
      [key: string]: CBR_Val
    } = {};
    for (const prop in co) {
      obj[prop] = co[prop].unmunge(bv["_" + prop]);
    }
    return V_Object(co)(obj);
  },
  paramType: (() => {
    const {ascLabels} = labelMaps(co);
    const tupFields = ascLabels.map((label) => `${co[label].paramType} _${label}`).join(',')
    return `tuple(${tupFields})`;
  })(),
});

const V_Object = <T>(
  co: {[key: string]: ETH_Ty<CBR_Val, T>}
) => (val: {[key: string]: unknown}): CBR_Object => {
  return T_Object(co).canonicalize(val);
}

const T_Data = <T>(
  co: {[key: string]: ETH_Ty<CBR_Val, T>}
): ETH_Ty<CBR_Data, Array<T>> => {
  // TODO: not duplicate between this and CBR.ts
  const {ascLabels, labelMap} = labelMaps(co);
  return {
    ...CBR.BT_Data(co),
    defaultValue: ((): CBR_Data => {
      const label = ascLabels[0];
      return [label, co[label].defaultValue];
      // return {ty, val: [label, co[label].defaultValue]};
    })(),
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
    munge: ([label, v]: CBR_Data): Array<T> => {
      const i = labelMap[label];
      const vals = ascLabels.map((label) => {
        const vco = co[label];
        return vco.munge(vco.defaultValue);
      });
      vals[i] = co[label].munge(v);
      const ret = [i as unknown as T];
      return ret.concat(vals);
    },
    // Note: when it comes back from solidity, vs behaves like an N+1-tuple,
    // but also has secret extra keys you can access,
    // based on the struct field names.
    // e.g. Maybe has keys vs["which"], vs["_None"], and vs["_Some"],
    // corresponding to    vs[0],       vs[1],       and vs[2] respectively.
    // We don't currently use these, but we could.
    unmunge: (vs: Array<T>): CBR_Data => {
      // @ts-ignore
      const ibn = T_UInt.unmunge(vs[0]);
      const i = bigNumberToNumber(ibn);
      const label = ascLabels[i];
      const val = vs[i + 1];
      return V_Data(co)([label, co[label].unmunge(val)]);
    },

    paramType: (() => {
      const {ascLabels} = labelMaps(co);
      // See comment on unmunge about field names that we could use but currently don't
      const optionTys = ascLabels.map((label) => `${co[label].paramType} _${label}`)
      const tupFields = [`${T_UInt.paramType} which`].concat(optionTys).join(',');
      return `tuple(${tupFields})`;
    })(),
  }
};

const V_Data = <T>(
  co: {[key: string]: ETH_Ty<CBR_Val, T>}
) => (val: [string, unknown]): CBR_Data => {
  return T_Data(co).canonicalize(val);
};

const T_Contract = {
  ...T_Address,
  name: 'Contract'
};

const addressEq = mkAddressEq(T_Address);
const digestEq = shared_backend.eq;

const T_Token = T_Address;
const tokenEq = addressEq;

const typeDefs: TypeDefs<AnyETH_Ty> = {
  T_Null,
  T_Bool,
  T_UInt,
  T_Bytes,
  T_Address,
  T_Contract,
  T_Digest,
  T_Token,
  T_Object,
  T_Data,
  T_Array,
  T_Tuple,
  T_Struct,
};

const arith: Arith = makeArith(UInt_max);

const emptyContractInfo = "0x00000000000000000000000000000000";

const stdlib: Stdlib_Backend_Base<AnyETH_Ty> = {
  ...shared_backend,
  ...arith,
  ...typeDefs,
  addressEq,
  // @ts-ignore
  digestEq,
  tokenEq,
  digest,
  UInt_max,
  emptyContractInfo,
};

// ...............................................
// It's the same as stdlib, but with convenient access to
// stdlib and typeDefs as bundles of bindings

// TODO: restore type annotation once types are in place
// const ethLikeCompiled: EthLikeCompiled = {
const ethLikeCompiled = {
  ...stdlib,
  typeDefs,
  stdlib,
};
return ethLikeCompiled;
}
