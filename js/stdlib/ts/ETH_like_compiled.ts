// ****************************************************************************
// standard library needed at runtime by compiled Reach programs
// ****************************************************************************

import ethers from 'ethers';
import * as shared_backend from './shared_backend';
import * as CBR from './CBR';

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

const digest = makeDigest((t:AnyETH_Ty, v:any) => {
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

const T_Null: ETH_Ty<CBR_Null, false> = {
  ...CBR.BT_Null,
  defaultValue: V_Null,
  // null is represented in solidity as false
  munge: (bv: CBR_Null): false => (void(bv), false),
  unmunge: (nv: false): CBR_Null => (void(nv), V_Null),
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
  ...CBR.BT_UInt,
  defaultValue: ethers.BigNumber.from(0),
  munge: (bv: CBR_UInt): BigNumber => bv,
  unmunge: (nv: BigNumber): CBR_UInt => V_UInt(nv),
  paramType: 'uint256',
};

const V_UInt = (n: BigNumber): CBR_UInt => {
  return T_UInt.canonicalize(n);
};

const T_Bytes = (len:number): ETH_Ty<CBR_Bytes, Array<number>> => {
  const me = {
    ...CBR.BT_Bytes(len),
    defaultValue: ''.padEnd(len, '\0'),
    munge: (bv: CBR_Bytes): Array<number> => Array.from(ethers.utils.toUtf8Bytes(bv)),
    unmunge: (nv: Array<number>) => me.canonicalize(hexToString(ethers.utils.hexlify(nv))),
    paramType: `uint8[${len}]`,
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
  munge: (bv: CBR_Object): any => {
    const obj: {
      [key: string]: any
    } = {};
    let none: boolean = true;
    for (const prop in co) {
      none = false;
      obj[prop] = co[prop].munge(bv[prop]);
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
      obj[prop] = co[prop].unmunge(bv[prop]);
    }
    return V_Object(co)(obj);
  },
  paramType: (() => {
    const {ascLabels} = labelMaps(co);
    const tupFields = ascLabels.map((label) => `${co[label].paramType} ${label}`).join(',')
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
      const i = vs[0] as unknown as number;
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

const addressEq = mkAddressEq(T_Address);

const T_Token = T_Address;
const tokenEq = addressEq;

const typeDefs: TypeDefs = {
  T_Null,
  T_Bool,
  T_UInt,
  T_Bytes,
  T_Address,
  T_Digest,
  T_Token,
  T_Object,
  T_Data,
  T_Array,
  T_Tuple,
  T_Struct,
};

const arith: Arith = makeArith(UInt_max);

const stdlib: Stdlib_Backend_Base<AnyETH_Ty> = {
  ...shared_backend,
  ...arith,
  ...typeDefs,
  addressEq,
  tokenEq,
  digest,
  UInt_max,
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
