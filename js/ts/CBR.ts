import ethers, {BigNumber} from 'ethers';

// Description of a Reach type
export type ReachTy =
  ['Null'] |
  ['Bool'] |
  ['UInt'] |
  ['Bytes'] |
  ['Address'] |
  ['Digest'] |
  ['Object', {[key: string]: ReachTy}] |
  ['Data', {[key: string]: ReachTy}] |
  ['Array', ReachTy, number] |
  ['Tuple', Array<ReachTy>] ;

// A Reach value of the given type
// .val: T is "CBR", canonical backend representation
export type CBR_Null = {
  ty: ['Null'],
  val: null,
}
export type CBR_Bool = {
  ty: ['Bool'],
  val: boolean,
}
export type CBR_UInt = {
  ty: ['UInt'],
  val: BigNumber,
}
export type CBR_Bytes = {
  ty: ['Bytes'],
  val: string,
}
export type CBR_Address = {
  ty: ['Address'],
  val: string,
}
export type CBR_Digest = {
  ty: ['Digest'],
  val: string,
}
// Too hard to teach ts the "real" type.
// T is the sum type of all children's types.
export type CBR_Object = {
  ty: ['Object', {[key: string]: ReachTy}],
  val: {[key: string]: CBR_Val},
}
export type CBR_Data = {
  ty: ['Data', {[key: string]: ReachTy}],
  val: [string, CBR_Val],
}
export type CBR_Array = {
  ty: ['Array', ReachTy, number], // size
  val: Array<CBR_Val>,
}
export type CBR_Tuple = {
  ty: ['Tuple', Array<ReachTy>],
  val: Array<CBR_Val>,
}

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
  CBR_Tuple ;

// UV = UserVal
// BV = BackendVal
export type BackendTy<T extends CBR_Val> = {
  name: string,
  ty: ReachTy,
  defaultValue: T,
  canonicalize: (uv: unknown) => T,
  // decanonicalize is just bv.val
}

export const BV_Null: CBR_Null = {
  ty: ['Null'], val: null,
}
export const BT_Null: BackendTy<CBR_Null> = {
  name: 'Null',
  ty: ['Null'],
  canonicalize: (val: unknown): CBR_Null => {
    // Doesn't check with triple eq; we're being lenient here
    if (val != null) {
      throw Error(`Expected null, but got ${JSON.stringify(val)}`);
    }
    return BV_Null;
  },
  // null is represented in solidity as false
  // munge: (v: null): false => { void(v); return false; },
  // unmunge: (v: false): null => { void(v); return null; },
  defaultValue: BV_Null,
};

export const BT_Bool: BackendTy<CBR_Bool> = {
  name: 'Bool',
  ty: ['Bool'],
  canonicalize: (val: unknown): CBR_Bool => {
    if (typeof(val) !== 'boolean') {
      throw Error(`Expected boolean, but got ${JSON.stringify(val)}`);
    }
    return {ty: ['Bool'], val};
  },
  // munge: (v: boolean): boolean => v,
  // unmunge: (v: boolean): boolean => v,
  defaultValue: {ty: ['Bool'], val: false},
};
export const BV_Bool = (val: boolean): CBR_Bool => {
  return BT_Bool.canonicalize(val);
};

export const BT_UInt: BackendTy<CBR_UInt> = {
  name: 'UInt',
  ty: ['UInt'],
  canonicalize: (uv: unknown): CBR_UInt => {
    try {
      const val = ethers.BigNumber.from(uv);
      return {ty: ['UInt'], val};
    } catch (e) {
      if (typeof(uv) === 'string') {
        throw Error(`String does not represent a BigNumber. ${JSON.stringify(uv)}`);
      } else {
        throw Error(`Expected BigNumber, number, or string, but got ${JSON.stringify(uv)}`);
      }
    }
  },
  // munge: (v: BigNumber): BigNumber => v,
  // TODO: double check:
  // It looks like munging BigNumber to string is no longer needed?
  // munge: (v) => v.toString(),
  // unmunge: (v: BigNumber): BigNumber => v,
  defaultValue: {ty: ['UInt'], val: ethers.BigNumber.from(0)},
};
export const BV_UInt = (val: BigNumber): CBR_UInt => {
  return BT_UInt.canonicalize(val);
};

export const BT_Bytes: BackendTy<CBR_Bytes> = {
  name: 'Bytes',
  ty: ['Bytes'],
  canonicalize: (val: unknown): CBR_Bytes => {
    if (typeof(val) !== 'string') {
      throw Error(`Bytes expected string, but got ${JSON.stringify(val)}`);
    }
    return {ty: ['Bytes'], val};
    // Hex the string later.
    // This version of the string should be human readable.

    // if (isHex(x)) {
    //   return x;
    // } else {
    //   return toHex(x);
    //   // TODO: fix things so this restriction is not necessary
    //   // throw Error(`Please use toHex on string sent to Reach: "${x}"`);
    // }
  },
  // munge: (v: string): string => v,
  // unmunge: (v: string): string => v,
  defaultValue: {ty: ['Bytes'], val: ''},
};
export const BV_Bytes = (val: string): CBR_Bytes => {
  return BT_Bytes.canonicalize(val);
};

// TODO: check digest length, or something similar?
export const BT_Digest: BackendTy<CBR_Digest> = {
  name: 'Digest',
  ty: ['Digest'],
  canonicalize: (val: unknown): CBR_Digest => {
    if (typeof val !== 'string') {
      throw Error(`${val} is not a valid digest`);
    }
    return {ty: ['Digest'], val};
  },
  defaultValue: {ty: ['Digest'], val: ethers.utils.keccak256([])},
}
/** @description You probably don't want to create a BV_Digest manually. */
export const BV_Digest = (val: string): CBR_Digest => {
  return BT_Digest.canonicalize(val);
}

export const BT_Address = (
  // TODO: better type?
  addressUnwrapper: ((x:any) => unknown)
): BackendTy<CBR_Address> => ({
  name: 'Address',
  ty: ['Address'],
  // TODO: just delete from here; leave it to net-specific impls?
  canonicalize: (x: unknown): CBR_Address => {
    // addressUnwrapper is expected to check network-specific things
    // like address length & general validity
    const mx = addressUnwrapper(x);
    if ( mx ) { x = mx; }
    if (typeof x !== 'string') {
      throw Error(`Address must be a string, but got: ${JSON.stringify(x)}`);
    } else if (x.slice(0, 2) !== '0x') {
      throw Error(`Address must start with 0x, but got: ${JSON.stringify(x)}`);
    } else if (!ethers.utils.isHexString(x)) {
      throw Error(`Address must be a valid hex string, but got: ${JSON.stringify(x)}`);
    }
    return {ty: ['Address'], val: x};
  },
  // munge: (v: string): string => v,
  // unmunge: (v: string): string => v,
  defaultValue: {ty: ['Address'], val: '0x' + Array(64).fill('0').join('')},
});
// Note: curried
export const BV_Address = (
  addressUnwrapper: ((x:any) => unknown)
) => (val: string): CBR_Address => {
  return BT_Address(addressUnwrapper).canonicalize(val);
}

export const BT_Array = (ctc: BackendTy<CBR_Val> , size: number): BackendTy<CBR_Array> => {
  // TODO: check ctc, sz for sanity
  const ty: ReachTy = ['Array', ctc.ty, size];
  return {
    name: `Array(${ctc.name}, ${size})`,
    ty: ['Array', ctc.ty, size],
    canonicalize: (args: any): CBR_Array => {
      if (!Array.isArray(args)) {
        throw Error(`Expected an Array, but got ${JSON.stringify(args)}`);
      }
      if (size != args.length) {
        throw Error(`Expected array of length ${size}, but got ${args.length}`);
      }
      return {ty, val: args.map((arg) => ctc.canonicalize(arg))};
    },
    // munge: (v: Array<T>): Array<any> => {
    //   return v.map((arg) => ctc.munge(arg));
    // },
    // unmunge: (v: Array<any>): Array<T> => {
    //   return v.map((arg) => ctc.unmunge(arg));
    // },
    defaultValue: {ty, val: Array(size).fill(ctc.defaultValue)},
  };
};
// Note: curried
/** @example BV_Array(BT_UInt, 3)([1, 2, 3]) */
export const BV_Array = (ctc: BackendTy<CBR_Val>, size: number) => (val: unknown[]): CBR_Array => {
  return BT_Array(ctc, size).canonicalize(val);
}

export const BT_Tuple = (ctcs: Array<BackendTy<CBR_Val>>): BackendTy<CBR_Tuple> => {
  // TODO: check ctcs for sanity
  const ty: ReachTy = ['Tuple', ctcs.map((ctc) => ctc.ty)];
  return {
    name: `Tuple(${ctcs.map((ctc) => ` ${ctc.name} `)})`,
    ty,
    canonicalize: (args: any): CBR_Tuple => {
      if (!Array.isArray(args)) {
        throw Error(`Expected a Tuple, but got ${JSON.stringify(args)}`);
      }
      if (ctcs.length != args.length) {
        throw Error(`Expected tuple of size ${ctcs.length}, but got ${args.length}`);
      }
      return {ty, val: args.map((arg, i) => ctcs[i].canonicalize(arg))};
    },
    // munge: (args: Array<T>): Array<any> => {
    //   return args.map((arg, i) => ctcs[i].munge(arg));
    // },
    // unmunge: (args: Array<any>): Array<T> => {
    //   return args.map((arg: any, i: number) => ctcs[i].unmunge(arg));
    // },
    defaultValue: {ty, val: ctcs.map(ctc => ctc.defaultValue)},
  };
};
// Note: curried
/** @example BV_Tuple([BT_UInt, BT_Bytes])([42, 'hello']) */
export const BV_Tuple = (ctcs: Array<BackendTy<CBR_Val>>) => (val: unknown[]) => {
  return BT_Tuple(ctcs).canonicalize(val);
};

export const BT_Object = (co: {
  [key: string]: BackendTy<CBR_Val>
}): BackendTy<CBR_Object> => {
  // TODO: check co for sanity
  const tys: {[key: string]: ReachTy} = {};
  for (const k in co) {
    tys[k] = co[k].ty;
  }
  const ty: ReachTy = ['Object', tys];
  return {
    name: `Object(${Object.keys(co).map((k) => ` ${k}: ${co[k].name} `)})`,
    ty,
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
      return {ty, val: obj};
    },
    // munge: (vo: {
    //   [key: string]: T
    // }): {
    //   [key: string]: any
    // } => {
    //   const obj: {
    //     [key: string]: any
    //   } = {};
    //   for (const prop in co) {
    //     obj[prop] = co[prop].munge(vo[prop]);
    //   }
    //   return obj;
    // },
    // // TODO: reduce duplication somehow
    // unmunge: (vo: {
    //   [key: string]: any
    // }): {
    //   [key: string]: T
    // } => {
    //   const obj: {
    //     [key: string]: T
    //   } = {};
    //   for (const prop in co) {
    //     obj[prop] = co[prop].unmunge(vo[prop]);
    //   }
    //   return obj;
    // },
    defaultValue: (() => {
      const obj: {
        [key: string]: CBR_Val
      } = {};
      for (const prop in co) {
        obj[prop] = co[prop].defaultValue;
      }
      return {ty, val: obj};
    })(),
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
  [key: string]: BackendTy<CBR_Val>
}): BackendTy<CBR_Data> => {
  const tys: {[key: string]: ReachTy} = {};
  for (const k in co) {
    tys[k] = co[k].ty;
  }
  const ty: ReachTy = ['Data', tys];
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
    ty,
    canonicalize: (io: unknown): CBR_Data => {
      if (!(Array.isArray(io) && io.length == 2 && typeof io[0] == 'string')) {
        throw Error(`Expected an array of length two to represent a data instance, but got ${JSON.stringify(io)}`);
      }
      const vn = io[0];
      if (!{}.hasOwnProperty.call(co, vn)) {
        throw Error(`Expected a variant in ${Object.keys(co)}, but got ${vn}`);
      }
      return {ty, val: [vn, co[vn].canonicalize(io[1])]};
    },
    // // Data representation in js is a 2-tuple:
    // // [label, val]
    // // where label : string
    // // and val : co[label]
    // //
    // // Data representation in solidity is an N+1-tuple: (actually a struct)
    // // [labelInt, v0, ..., vN]
    // // where labelInt : number, 0 <= labelInt < N
    // // vN : co[ascLabels[i]]
    // //
    // munge: (vt: [string, T]): Array<any> => {
    //   // Typescript is stupid about destructuring tuple tupes =(
    //   const label = vt[0];
    //   const v = vt[1];
    //   const i = labelMap[label];
    //   const vals = ascLabels.map((label) => {
    //     const vco = co[label];
    //     return vco.munge(vco.defaultValue);
    //   });
    //   vals[i] = co[label].munge(v);
    //   return [i].concat(vals);
    // },
    // // Note: when it comes back from solidity, vs behaves like an N+1-tuple,
    // // but also has secret extra keys you can access,
    // // based on the struct field names.
    // // e.g. Maybe has keys vs["which"], vs["_None"], and vs["_Some"],
    // // corresponding to    vs[0],       vs[1],       and vs[2] respectively.
    // // We don't currently use these, but we could.
    // unmunge: (vs: Array<any>): [string, T] => {
    //   const i = vs[0];
    //   const label = ascLabels[i];
    //   const val = vs[i + 1];
    //   return [label, co[label].unmunge(val)];
    // },
    defaultValue: ((): CBR_Data => {
      const label = ascLabels[0];
      return {ty, val: [label, co[label].defaultValue]};
    })(),
  };
};
/** @example BV_Data({x: BT_UInt, y: BT_Bytes})(['x', 3]); */
export const BV_Data = (co: {
  [key: string]: BackendTy<CBR_Val>,
}) => (val: [string, unknown]) => {
  return BT_Data(co).canonicalize(val);
}
