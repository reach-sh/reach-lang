import { ethers } from 'ethers';
import { checkedBigNumberify } from './shared_backend';
import { debug, j2s, labelMaps, hasProp, isUint8Array } from './shared_impl';
import buffer from 'buffer';
const Buffer = buffer.Buffer;
// "CBR", canonical backend representation

type BigNumber = ethers.BigNumber;
const BigNumber = ethers.BigNumber;
export const bigNumberify = (x: any): BigNumber => {
  const xp = typeof x === 'number' ? x.toString() : x;
  return BigNumber.from(xp);
};
export const bigNumberToNumber = (x: any) =>
  bigNumberify(x).toNumber();

// A Reach value of the given type
export type CBR_Null = null;
export type CBR_Bool = boolean;
export type CBR_UInt = BigNumber;
export type CBR_Bytes = Uint8Array | string;
export type CBR_Address = string;
export type CBR_Digest = string;
export type CBR_Object = {[key: string]: CBR_Val};
export type CBR_Data = [string, CBR_Val];
export type CBR_Array = Array<CBR_Val>;
export type CBR_Tuple = Array<CBR_Val>;
export type CBR_Struct = {[key: string]: CBR_Val};

export type CBR_Val =
  CBR_Null |
  CBR_Bool |
  CBR_UInt |
  CBR_Bytes |
  CBR_Address |
  CBR_Digest |
  CBR_Object |
  CBR_Data |
  CBR_Array |
  CBR_Tuple |
  CBR_Struct;

// UV = UserVal
// BV = BackendVal
export interface BackendTy<T extends CBR_Val> {
  name: string,
  canonicalize: (uv: unknown) => T,
  defaultValue: T,
  toString: () => string,
};

export const BV_Null: CBR_Null = null;
export const BT_Null: BackendTy<CBR_Null> = {
  name: 'Null',
  defaultValue: BV_Null,
  canonicalize: (val: unknown): CBR_Null => {
    // Doesn't check with triple eq; we're being lenient here
    if (val != null) {
      throw Error(`Expected null, but got ${j2s(val)}`);
    }
    return BV_Null;
  },
};

export const BT_Bool: BackendTy<CBR_Bool> = {
  name: 'Bool',
  defaultValue: false,
  canonicalize: (val: unknown): CBR_Bool => {
    if (typeof(val) !== 'boolean') {
      throw Error(`Expected boolean, but got ${j2s(val)}`);
    }
    return val;
  },
};
export const BV_Bool = (val: boolean): CBR_Bool => {
  return BT_Bool.canonicalize(val);
};

export const BT_UInt = (max: BigNumber): BackendTy<CBR_UInt> => ({
  name: 'UInt',
  defaultValue: ethers.BigNumber.from(0),
  canonicalize: (uv: unknown): CBR_UInt => {
    try {
      // Note: going through toString handles a lot of numeric representations
      // that BigNumber doesn't handle automatically.
      const uvs =
        // @ts-ignore
        uv?.type === 'BigNumber' ? uv :
        // @ts-ignore
        typeof uv?.toString === 'function' ? uv.toString() :
        /* else */ uv;
      return checkedBigNumberify('stdlib:CBR:BT_UInt', max, uvs);
    } catch (e) {
      if (typeof(uv) === 'string') {
        throw Error(`String does not represent a BigNumber. ${j2s(uv)}`);
      }
      throw e;
    }
  },
});

export const BV_UInt = (val: BigNumber, max: BigNumber): CBR_UInt => {
  return BT_UInt(max).canonicalize(val);
};

const zpad = (len: number, b: Buffer): Buffer => {
  const res = Buffer.alloc(len, 0);
  b.copy(res);
  return res;
};
type BLabel = 'string' | 'hex string' | 'Uint8Array' | 'unknown';
export const arr_to_buf = (s: Uint8Array): Buffer => Buffer.from(s);
export const str_to_buf = (s: string): Buffer => Buffer.from(s);
export const hex_to_buf = (s: string): Buffer => Buffer.from(s.slice(2), 'hex');
export const buf_to_arr = (b: Buffer): Uint8Array => new Uint8Array(b);
export const buf_to_str = (b: Buffer): string => b.toString();
export const buf_to_hex = (b: Buffer): string => '0x' + b.toString('hex');
export const unk_to_buf = (val: unknown): [BLabel, Buffer] => {
  if (typeof val === 'string') {
    return val.slice(0, 2) === '0x'
      ? ['hex string', hex_to_buf(val)]
      : ['string', str_to_buf(val)];
  } else if (isUint8Array(val)) {
    return ['Uint8Array', arr_to_buf(val as Uint8Array)];
  } else {
    return ['unknown', str_to_buf('')];
  }
};
export const BT_Bytes = (len: number|BigNumber): BackendTy<CBR_Bytes> => ({
  name: `Bytes(${len})`,
  defaultValue: buf_to_str(zpad(bigNumberToNumber(len), str_to_buf(''))),
  canonicalize: (val: unknown): CBR_Bytes => {
    const [label, b] = unk_to_buf(val);
    debug(`Canonicalize bytes:`, val, `=>`, label, b);
    const alen = b.length;
    const lenn = bigNumberToNumber(len);
    if (alen > lenn) {
      throw Error(`Bytes(${lenn}) must be less than or equal to ${lenn} bytes, but given ${label} of ${alen} bytes`);
    }
    const zb = zpad(lenn, b);
    if      (label === 'hex string') { return buf_to_hex(zb); }
    else if (label === 'string')      { return buf_to_str(zb); }
    else if (label === 'Uint8Array') { return buf_to_arr(zb); }
    else {
      throw Error(`Bytes expected string or Uint8Array, but got ${j2s(val)}: ${typeof val}`);
    }
  },
});

export const BT_BytesDyn: BackendTy<CBR_Bytes> = ({
  name: `BytesDyn`,
  defaultValue: '',
  canonicalize: (val: unknown): CBR_Bytes => {
    if (typeof val == 'string') {
      return val;
    } else if (isUint8Array(val)) {
      return val as Uint8Array;
    } else {
      throw Error(`BytesDyn expected string or Uint8Array, but got ${j2s(val)}`);
    }
  },
});

export const BT_StringDyn: BackendTy<CBR_Bytes> = ({
  name: `StringDyn`,
  defaultValue: '',
  canonicalize: (val: unknown): CBR_Bytes => {
    if (typeof(val) !== 'string') {
      throw Error(`StringDyn expected string, but got ${j2s(val)}`);
    }
    return val;
  },
});

// TODO: check digest length, or something similar?
// That's probably best left to connector-specific code.
export const BT_Digest: BackendTy<CBR_Digest> = {
  name: 'Digest',
  defaultValue: ''.padEnd(32, '\0'), // XXX hack
  canonicalize: (val: unknown): CBR_Digest => {
    if (typeof val !== 'string') {
      throw Error(`${j2s(val)} is not a valid digest`);
    }
    return val;
  },
}
/** @description You probably don't want to create a BV_Digest manually. */
export const BV_Digest = (val: string): CBR_Digest => {
  return BT_Digest.canonicalize(val);
}

export const BT_Address: BackendTy<CBR_Address> = ({
  name: 'Address',
  defaultValue: ''.padEnd(32, '\0'), // XXX hack
  canonicalize: (val: unknown): CBR_Address => {
    if (typeof val !== 'string') {
      throw Error(`Address must be a string, but got: ${j2s(val)}`);
    } else if (val.slice(0, 2) !== '0x') {
      throw Error(`Address must start with 0x, but got: ${j2s(val)}`);
    } else if (!ethers.utils.isHexString(val)) {
      throw Error(`Address must be a valid hex string, but got: ${j2s(val)}`);
    }
    return val;
  },
});
// XXX: don't use this. Use net-specific ones
export const BV_Address = (val: string): CBR_Address => {
  return BT_Address.canonicalize(val);
}

export const BT_Array = (ctc: BackendTy<CBR_Val> , size: number): BackendTy<CBR_Array> => {
  // TODO: check ctc, sz for sanity
  return {
    name: `Array(${ctc.name}, ${size})`,
    defaultValue: Array(size).fill(ctc.defaultValue),
    canonicalize: (args: any): CBR_Array => {
      if (!Array.isArray(args)) {
        throw Error(`Expected an Array, but got ${j2s(args)}`);
      }
      if (size != args.length) {
        throw Error(`Expected array of length ${size}, but got ${args.length}`);
      }
      const parr = new Array(size);
      for ( let i = 0; i < size; i++ ) {
        parr[i] = ctc.canonicalize(args[i]);
      }
      return parr;
    },
  };
};
// Note: curried
/** @example BV_Array(BT_UInt, 3)([1, 2, 3]) */
export const BV_Array = (ctc: BackendTy<CBR_Val>, size: number) => (val: unknown[]): CBR_Array => {
  return BT_Array(ctc, size).canonicalize(val);
}

export const BT_Tuple = (ctcs: Array<BackendTy<CBR_Val>>): BackendTy<CBR_Tuple> => {
  // TODO: check ctcs for sanity
  return {
    name: `Tuple(${ctcs.map((ctc) => ` ${ctc.name} `)})`,
    defaultValue: ctcs.map(ctc => ctc.defaultValue),
    canonicalize: (args: any): CBR_Tuple => {
      if (!Array.isArray(args)) {
        throw Error(`Expected a Tuple, but got ${j2s(args)}`);
      }
      if (ctcs.length != args.length) {
        throw Error(`Expected tuple of size ${ctcs.length}, but got ${args.length}`);
      }
      const val = args.map((arg, i) => ctcs[i].canonicalize(arg));
      return val;
    },
  };
};
// Note: curried
/** @example BV_Tuple([BT_UInt, BT_Bytes])([42, 'hello']) */
export const BV_Tuple = (ctcs: Array<BackendTy<CBR_Val>>) => (val: unknown[]) => {
  return BT_Tuple(ctcs).canonicalize(val);
};

export const BT_Struct = (ctcs: Array<[string, BackendTy<CBR_Val>]>): BackendTy<CBR_Struct> => {
  return {
    name: `Struct([${ctcs.map(([k, ctc]) => ` [${k}, ${ctc.name}] `)}])`,
    defaultValue: (() => {
      const obj: {[key: string]: CBR_Val} = {};
      ctcs.forEach(([prop, co]) => {
        obj[prop] = co.defaultValue;
      });
      return obj;
    })(),
    canonicalize: (arg: any): CBR_Struct => {
      const obj: {
        [key: string]: CBR_Val
      } = {};
      ctcs.forEach(([k, ctc], i) => {
        obj[k] = ctc.canonicalize(Array.isArray(arg) ? arg[i] : arg[k]);
      });
      return obj;
    },
  };
};

export const BV_Struct = (ctcs: Array<[string, BackendTy<CBR_Val>]>) => (val:any) => {
  return BT_Struct(ctcs).canonicalize(val);
};

export const BT_Object = (co: {
  [key: string]: BackendTy<CBR_Val>
}): BackendTy<CBR_Object> => {
  // TODO: check co for sanity
  return {
    name: `Object(${Object.keys(co).map((k) => ` ${k}: ${co[k].name} `)})`,
    defaultValue: (() => {
      const obj: {[key: string]: CBR_Val} = {};
      for (const prop in co) {
        obj[prop] = co[prop].defaultValue;
      }
      return obj;
    })(),
    canonicalize: (vo: any): CBR_Object => {
      if (typeof(vo) !== 'object') {
        throw Error(`Expected object, but got ${j2s(vo)}`);
      }
      const obj: {
        [key: string]: CBR_Val
      } = {};
      for (const prop in co) {
        if (!hasProp(vo, prop)) {
          throw Error(`Expected prop ${prop}, but didn't find it in ${Object.keys(vo)}`);
        }
        obj[prop] = co[prop].canonicalize(vo[prop]);
      }
      return obj;
    },
  };
};
// Note: curried
/** @example BV_Object({x: BT_UInt})({x: 3}) */
export const BV_Object = (
  co: {[key: string]: BackendTy<CBR_Val>}
) => (val: {[key: string]: unknown}) => {
  return BT_Object(co).canonicalize(val);
}

export const BT_Data = (co: {
  [key: string]: BackendTy<CBR_Val>,
}): BackendTy<CBR_Data> => {
  // TODO: check co for sanity
  const {ascLabels} = labelMaps(co);
  return {
    name: `Data(${Object.keys(co).map((k) => ` ${k}: ${co[k].name} `)})`,
    defaultValue: ((): CBR_Data => {
      const label = ascLabels[0];
      return [label, co[label].defaultValue];
      // return {ty, val: [label, co[label].defaultValue]};
    })(),
    canonicalize: (io: unknown): CBR_Data => {
      if (!(Array.isArray(io) && io.length == 2 && typeof io[0] == 'string')) {
        throw Error(`Expected an array of length two to represent a data instance, but got ${j2s(io)}`);
      }
      const vn = io[0];
      if (!hasProp(co, vn)) {
        throw Error(`Expected a variant in ${Object.keys(co)}, but got ${vn}`);
      }
      return [vn, co[vn].canonicalize(io[1])];
    },
  };
};
/** @example BV_Data({x: BT_UInt, y: BT_Bytes})(['x', 3]); */
export const BV_Data = (co: {
  [key: string]: BackendTy<CBR_Val>,
}) => (val: [string, unknown]) => {
  return BT_Data(co).canonicalize(val);
}
