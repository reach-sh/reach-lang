import ethers from 'ethers';
// "CBR", canonical backend representation

type BigNumber = ethers.BigNumber;
const BigNumber = ethers.BigNumber;
export const bigNumberify = (x: any): BigNumber => BigNumber.from(x);
export const bigNumberToNumber = (x: any) =>
  bigNumberify(x).toNumber();

// A Reach value of the given type
export type CBR_Null = null;
export type CBR_Bool = boolean;
export type CBR_UInt = BigNumber;
export type CBR_Bytes = string;
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
export type BackendTy<T extends CBR_Val> = {
  name: string,
  canonicalize: (uv: unknown) => T,
}

export const BV_Null: CBR_Null = null;
export const BT_Null: BackendTy<CBR_Null> = {
  name: 'Null',
  canonicalize: (val: unknown): CBR_Null => {
    // Doesn't check with triple eq; we're being lenient here
    if (val != null) {
      throw Error(`Expected null, but got ${JSON.stringify(val)}`);
    }
    return BV_Null;
  },
};

export const BT_Bool: BackendTy<CBR_Bool> = {
  name: 'Bool',
  canonicalize: (val: unknown): CBR_Bool => {
    if (typeof(val) !== 'boolean') {
      throw Error(`Expected boolean, but got ${JSON.stringify(val)}`);
    }
    return val;
  },
};
export const BV_Bool = (val: boolean): CBR_Bool => {
  return BT_Bool.canonicalize(val);
};

export const BT_UInt: BackendTy<CBR_UInt> = {
  name: 'UInt',
  canonicalize: (uv: unknown): CBR_UInt => {
    try {
      const val = ethers.BigNumber.from(uv);
      return val;
    } catch (e) {
      if (typeof(uv) === 'string') {
        throw Error(`String does not represent a BigNumber. ${JSON.stringify(uv)}`);
      } else {
        throw Error(`Expected BigNumber, number, or string, but got ${JSON.stringify(uv)}`);
      }
    }
  },
};
export const BV_UInt = (val: BigNumber): CBR_UInt => {
  return BT_UInt.canonicalize(val);
};

export const BT_Bytes = (len: number): BackendTy<CBR_Bytes> => ({
  name: `Bytes(${len})`,
  canonicalize: (val: unknown): CBR_Bytes => {
    const lenn = bigNumberToNumber(len);
    if (typeof(val) !== 'string') {
      throw Error(`Bytes expected string, but got ${JSON.stringify(val)}`);
    }
    const checkLen = (label:string, alen:number, fill:string): string => {
      if ( val.length > alen ) {
        throw Error(`Bytes(${len}) must be a ${label}string less than or equal to ${alen}, but given ${label}string of length ${val.length}`);
      }
      return val.padEnd(alen, fill);
    };
    if ( val.slice(0,2) === '0x' ) {
      return checkLen('hex ', lenn*2+2, '0');
    } else {
      return checkLen('', lenn, '\0');
    }
  },
});

// TODO: check digest length, or something similar?
// That's probably best left to connector-specific code.
export const BT_Digest: BackendTy<CBR_Digest> = {
  name: 'Digest',
  canonicalize: (val: unknown): CBR_Digest => {
    if (typeof val !== 'string') {
      throw Error(`${JSON.stringify(val)} is not a valid digest`);
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
  canonicalize: (val: unknown): CBR_Address => {
    if (typeof val !== 'string') {
      throw Error(`Address must be a string, but got: ${JSON.stringify(val)}`);
    } else if (val.slice(0, 2) !== '0x') {
      throw Error(`Address must start with 0x, but got: ${JSON.stringify(val)}`);
    } else if (!ethers.utils.isHexString(val)) {
      throw Error(`Address must be a valid hex string, but got: ${JSON.stringify(val)}`);
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
    canonicalize: (args: any): CBR_Array => {
      if (!Array.isArray(args)) {
        throw Error(`Expected an Array, but got ${JSON.stringify(args)}`);
      }
      if (size != args.length) {
        throw Error(`Expected array of length ${size}, but got ${args.length}`);
      }
      const val = args.map((arg) => ctc.canonicalize(arg));
      return val;
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
    canonicalize: (args: any): CBR_Tuple => {
      if (!Array.isArray(args)) {
        throw Error(`Expected a Tuple, but got ${JSON.stringify(args)}`);
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
    canonicalize: (vo: any): CBR_Object => {
      if (typeof(vo) !== 'object') {
        throw Error(`Expected object, but got ${JSON.stringify(vo)}`);
      }
      const obj: {
        [key: string]: CBR_Val
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
  return {
    name: `Data(${Object.keys(co).map((k) => ` ${k}: ${co[k].name} `)})`,
    canonicalize: (io: unknown): CBR_Data => {
      if (!(Array.isArray(io) && io.length == 2 && typeof io[0] == 'string')) {
        throw Error(`Expected an array of length two to represent a data instance, but got ${JSON.stringify(io)}`);
      }
      const vn = io[0];
      if (!{}.hasOwnProperty.call(co, vn)) {
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
