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
  defineSimStuff,
} from './shared_sim';
import {
  debug,
  labelMaps,
  MkPayAmt,
  makeDigest,
  hexToString,
  mkAddressEq,
  makeArith,
  j2s,
  UInt256_max,
  canonicalToBytes,
} from './shared_impl';
export type { // =>
  ETH_Ty,
  AnyETH_Ty,
}

// Types
export type Token = CBR_Address;
export type PayAmt = MkPayAmt<Token>;
export type ContractInfo = string;
type ConnectorTy = AnyETH_Ty;

// TODO: restore return type annotation once types are in place
export function makeEthLikeCompiled(ethLikeCompiledArgs: EthLikeCompiledArgs) {
// ...............................................
const {
  T_Address,
} = ethLikeCompiledArgs;

const UInt_max: BigNumber = UInt256_max;

const digest = makeDigest('keccak256', (ts:any[], vs:any[]) => {
  // Note: abiCoder.encode doesn't correctly handle an empty tuple type
  if (Array.isArray(ts) && ts.length === 0) {
    if (Array.isArray(vs) && vs.length === 0) {
      return vs;
    } else {
      throw Error(`impossible: digest tuple() with non-empty array: ${j2s(vs)}`);
    }
  }
  const pts = ts.map((t:any) => t.paramType);
  const mvs = ts.map((t:any, i:number) => t.munge(vs[i]));
  debug('digest prep', { ts, vs, pts, mvs });
  return ethers.utils.defaultAbiCoder.encode(pts, mvs);
});

const V_Null: CBR_Null = null;

const instEthTy = (ty: any) =>
  ({ ...ty, toString: () => ty.paramType });

// null is represented in solidity as false
const T_Null: ETH_Ty<CBR_Null, false> = instEthTy({
  ...CBR.BT_Null,
  munge: (bv: CBR_Null): false => (void(bv), false),
  unmunge: (nv: false): CBR_Null => (void(nv), V_Null),
  paramType: 'bool',
  isBaseType: true,
});

const T_Bool: ETH_Ty<CBR_Bool, boolean> = instEthTy({
  ...CBR.BT_Bool,
  munge: (bv: CBR_Bool): boolean => bv,
  unmunge: (nv: boolean): CBR_Bool => V_Bool(nv),
  paramType: 'bool',
  isBaseType: true,
});

const V_Bool = (b: boolean): CBR_Bool => {
  return T_Bool.canonicalize(b);
};

const T_UInt: ETH_Ty<CBR_UInt, BigNumber> = instEthTy({
  ...CBR.BT_UInt(UInt_max),
  munge: (bv: CBR_UInt): BigNumber => bigNumberify(bv),
  unmunge: (nv: BigNumber): CBR_UInt => V_UInt(nv),
  paramType: 'uint256',
  isBaseType: true,
});

const T_UInt256 = T_UInt;

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

const byteChunkSize = 32;
type ETH_Bytes = Array<number> | Array<Array<number>>;
const T_Bytes = (len:number): ETH_Ty<CBR_Bytes, ETH_Bytes> => {
  const me = {
    ...CBR.BT_Bytes(len),
    isBaseType: len <= byteChunkSize,
    munge: ((bv: CBR_Bytes): ETH_Bytes => {
      const bs = Array.from(canonicalToBytes(bv));
      return (len <= byteChunkSize) ? bs : splitToChunks(bs, byteChunkSize);
    }),
    unmunge: ((nvs: ETH_Bytes): CBR_Bytes => {
      const go = (nv:any, i: number) => {
        const r = ethers.utils.hexlify(unBigInt(nv));
        return (i > 0 && r.startsWith('0x')) ? r.slice(2) : r;
      }
      return hexToString((len <= byteChunkSize)
              ? go(nvs, 0)
              : nvs.map(go).join(''));
    }),
    paramType: (() => {
      let n = len;
      const fs = [];
      while ( 0 < n ) {
        const ell = Math.min(byteChunkSize, n);
        fs.push(`bytes${ell}`);
        n = n - ell;
      }
      return (len <= byteChunkSize)
        ? fs[0]
        : `tuple(${fs.join(',')})`;
    })(),
  };
  return instEthTy(me);
};

type ETH_BytesDyn = Array<number>;
const T_BytesDyn: ETH_Ty<CBR_Bytes, ETH_BytesDyn> = (() => {
  const me = {
    ...CBR.BT_BytesDyn,
    isBaseType: true,
    munge: ((bv: CBR_Bytes): ETH_BytesDyn => {
      return Array.from(canonicalToBytes(bv));
    }),
    unmunge: ((nv: ETH_BytesDyn): CBR_Bytes => {
      const nv_s = hexToString(ethers.utils.hexlify(unBigInt(nv)));
      return me.canonicalize(nv_s);
    }),
    paramType: 'bytes',
  };
  return instEthTy(me);
})();

type ETH_StringDyn = string;
const T_StringDyn: ETH_Ty<CBR_Bytes, ETH_StringDyn> = (() => {
  const me = {
    ...CBR.BT_StringDyn,
    isBaseType: true,
    munge: ((bv: CBR_Bytes): ETH_StringDyn => {
      return typeof bv == 'string' ? bv : ethers.utils.toUtf8String(bv);
    }),
    unmunge: ((nv: ETH_StringDyn): CBR_Bytes => {
      return nv;
    }),
    paramType: 'string',
  };
  return instEthTy(me);
})();

const T_Digest: ETH_Ty<CBR_Digest, BigNumber> = instEthTy({
  ...CBR.BT_Digest,
  isBaseType: true,
  defaultValue: ethers.utils.keccak256([]),
  munge: (bv: CBR_Digest): BigNumber => ethers.BigNumber.from(bv),
  // XXX likely not the correct unmunge type?
  unmunge: (nv: BigNumber): CBR_Digest => V_Digest(nv.toHexString()),
  paramType: 'uint256',
});
const V_Digest = (s: string): CBR_Digest => {
  return T_Digest.canonicalize(s);
};

const T_Array = <T>(
  ctc: ETH_Ty<CBR_Val, T>,
  size_i: unknown,
): ETH_Ty<CBR_Array, Array<T>> => {
  const size = bigNumberToNumber(bigNumberify(size_i));
  return instEthTy({
    ...CBR.BT_Array(ctc, size),
    isBaseType: false,
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
};

const V_Array = <T>(
  ctc: ETH_Ty<CBR_Val, T>,
  size: number,
) => (val: Array<unknown>): CBR_Array => {
  return T_Array(ctc, size).canonicalize(val);
};

// XXX fix me Dan, I'm type checking wrong!
const T_Tuple = <T>(
  ctcs: Array<ETH_Ty<CBR_Val, T>>,
): ETH_Ty<CBR_Tuple, Array<T>> => instEthTy({
  ...CBR.BT_Tuple(ctcs),
  isBaseType: false,
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
): ETH_Ty<CBR_Struct, Array<T>> => instEthTy({
  ...CBR.BT_Struct(ctcs),
  isBaseType: false,
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
): ETH_Ty<CBR_Object, {[key: string]: T}> => instEthTy({
  ...CBR.BT_Object(co),
  isBaseType: false,
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
  return instEthTy({
    ...CBR.BT_Data(co),
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
    isBaseType: false,
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
  })
};

const V_Data = <T>(
  co: {[key: string]: ETH_Ty<CBR_Val, T>}
) => (val: [string, unknown]): CBR_Data => {
  return T_Data(co).canonicalize(val);
};

const T_Contract = instEthTy({
  ...T_Address,
  name: 'Contract'
});

const addressEq = mkAddressEq(T_Address);
const ctcAddrEq = (x:unknown, y:unknown) => {
  debug('ctcAddrEq', {x, y});
  return addressEq(x, y);
};
const digestEq = shared_backend.eq;
const digest_xor = shared_backend.digest_xor;
const bytes_xor = shared_backend.bytes_xor;
const btoiLast8 = shared_backend.btoiLast8;

const T_Token = T_Address;
const tokenEq = addressEq;

const typeDefs: TypeDefs<AnyETH_Ty> = {
  T_Null,
  T_Bool,
  T_UInt,
  T_UInt256,
  T_Bytes,
  T_BytesDyn,
  T_StringDyn,
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

const stdlib: Stdlib_Backend_Base<Token, ContractInfo, ConnectorTy> = {
  ...shared_backend,
  ...defineSimStuff<Token, ContractInfo, ConnectorTy>(),
  ...arith,
  ...typeDefs,
  addressEq,
  ctcAddrEq,
  // @ts-ignore
  digestEq,
  digest_xor,
  bytes_xor,
  btoiLast8,
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
