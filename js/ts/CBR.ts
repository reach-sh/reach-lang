import ethers, {BigNumber} from 'ethers';

// Description of a Reach type
// export type ReachTy =
//   ['Null'] |
//   ['Bool'] |
//   ['UInt'] |
//   ['Bytes'] |
//   ['Address'] |
//   ['Digest'] |
//   ['Object', {[key: string]: ReachTy}] |
//   ['Data', {[key: string]: ReachTy}] |
//   ['Array', ReachTy, number] |
//   ['Tuple', Array<ReachTy>] ;

// A Reach value of the given type
// .val: T is "CBR", canonical backend representation
export type CBR_Null = null;
// {
//   ty: ['Null'],
//   val: null,
// }
export type CBR_Bool = boolean;
// {
//   ty: ['Bool'],
//   val: boolean,
// }
export type CBR_UInt = BigNumber;
// {
//   ty: ['UInt'],
//   val: BigNumber,
// }
export type CBR_Bytes = string;
// {
//   ty: ['Bytes'],
//   val: string,
// }
export type CBR_Address = string;
// {
//   ty: ['Address'],
//   val: string,
// }
export type CBR_Digest = string;
// {
//   ty: ['Digest'],
//   val: string,
// }
// Too hard to teach ts the "real" type.
// T is the sum type of all children's types.
export type CBR_Object = {
  [key: string]: CBR_Val,
};
// {
//   ty: ['Object', {[key: string]: ReachTy}],
//   val: {[key: string]: CBR_Val},
// }
export type CBR_Data = [string, CBR_Val];
// {
//   ty: ['Data', {[key: string]: ReachTy}],
//   val: [string, CBR_Val],
// }
export type CBR_Array = Array<CBR_Val>;
// {
//   ty: ['Array', ReachTy, number], // size
//   val: Array<CBR_Val>,
// }
export type CBR_Tuple = Array<CBR_Val>;
// {
//   ty: ['Tuple', Array<ReachTy>],
//   val: Array<CBR_Val>,
// }

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
  // ty: ReachTy,
  defaultValue: T,
  canonicalize: (uv: unknown) => T,
}

export const BV_Null: CBR_Null = null;
// {
//   ty: ['Null'], val: null,
// }
export const BT_Null: BackendTy<CBR_Null> = {
  name: 'Null',
  // ty: ['Null'],
  canonicalize: (val: unknown): CBR_Null => {
    // Doesn't check with triple eq; we're being lenient here
    if (val != null) {
      throw Error(`Expected null, but got ${JSON.stringify(val)}`);
    }
    return BV_Null;
  },
  // null is represented in solidity as false
  defaultValue: BV_Null,
};

export const BT_Bool: BackendTy<CBR_Bool> = {
  name: 'Bool',
  // ty: ['Bool'],
  canonicalize: (val: unknown): CBR_Bool => {
    if (typeof(val) !== 'boolean') {
      throw Error(`Expected boolean, but got ${JSON.stringify(val)}`);
    }
    return val;
    // return {ty: ['Bool'], val};
  },
  defaultValue: false,
  // defaultValue: {ty: ['Bool'], val: false},
};
export const BV_Bool = (val: boolean): CBR_Bool => {
  return BT_Bool.canonicalize(val);
};

export const BT_UInt: BackendTy<CBR_UInt> = {
  name: 'UInt',
  // ty: ['UInt'],
  canonicalize: (uv: unknown): CBR_UInt => {
    try {
      const val = ethers.BigNumber.from(uv);
      return val;
      // return {ty: ['UInt'], val};
    } catch (e) {
      if (typeof(uv) === 'string') {
        throw Error(`String does not represent a BigNumber. ${JSON.stringify(uv)}`);
      } else {
        throw Error(`Expected BigNumber, number, or string, but got ${JSON.stringify(uv)}`);
      }
    }
  },
  defaultValue: ethers.BigNumber.from(0),
  // defaultValue: {ty: ['UInt'], val: ethers.BigNumber.from(0)},
};
export const BV_UInt = (val: BigNumber): CBR_UInt => {
  return BT_UInt.canonicalize(val);
};

export const BT_Bytes: BackendTy<CBR_Bytes> = {
  name: 'Bytes',
  // ty: ['Bytes'],
  canonicalize: (val: unknown): CBR_Bytes => {
    if (typeof(val) !== 'string') {
      throw Error(`Bytes expected string, but got ${JSON.stringify(val)}`);
    }
    return val;
  },
  defaultValue: '',
  // defaultValue: {ty: ['Bytes'], val: ''},
};
export const BV_Bytes = (val: string): CBR_Bytes => {
  return BT_Bytes.canonicalize(val);
};

// TODO: check digest length, or something similar?
export const BT_Digest: BackendTy<CBR_Digest> = {
  name: 'Digest',
  // ty: ['Digest'],
  canonicalize: (val: unknown): CBR_Digest => {
    if (typeof val !== 'string') {
      throw Error(`${val} is not a valid digest`);
    }
    return val;
    // return {ty: ['Digest'], val};
  },
  defaultValue: ethers.utils.keccak256([]),
  // defaultValue: {ty: ['Digest'], val: ethers.utils.keccak256([])},
}
/** @description You probably don't want to create a BV_Digest manually. */
export const BV_Digest = (val: string): CBR_Digest => {
  return BT_Digest.canonicalize(val);
}

export const BT_Address: BackendTy<CBR_Address> = ({
  name: 'Address',
  // ty: ['Address'],
  // TODO: just delete from here; leave it to net-specific impls?
  canonicalize: (val: unknown): CBR_Address => {
    if (typeof val !== 'string') {
      throw Error(`Address must be a string, but got: ${JSON.stringify(val)}`);
    } else if (val.slice(0, 2) !== '0x') {
      throw Error(`Address must start with 0x, but got: ${JSON.stringify(val)}`);
    } else if (!ethers.utils.isHexString(val)) {
      throw Error(`Address must be a valid hex string, but got: ${JSON.stringify(val)}`);
    }
    return val;
    // return {ty: ['Address'], val: x};
  },
  defaultValue: '0x' + Array(64).fill('0').join(''),
  // defaultValue: {ty: ['Address'], val: '0x' + Array(64).fill('0').join('')},
});
// Note: curried
// XXX: don't use this. Use net-specific ones
export const BV_Address = (val: string): CBR_Address => {
  return BT_Address.canonicalize(val);
}

export const BT_Array = (ctc: BackendTy<CBR_Val> , size: number): BackendTy<CBR_Array> => {
  // TODO: check ctc, sz for sanity
  // const ty: ReachTy = ['Array', ctc.ty, size];
  return {
    name: `Array(${ctc.name}, ${size})`,
    // ty: ['Array', ctc.ty, size],
    canonicalize: (args: any): CBR_Array => {
      if (!Array.isArray(args)) {
        throw Error(`Expected an Array, but got ${JSON.stringify(args)}`);
      }
      if (size != args.length) {
        throw Error(`Expected array of length ${size}, but got ${args.length}`);
      }
      const val = args.map((arg) => ctc.canonicalize(arg));
      return val;
      // return {ty, val};
    },
    defaultValue: Array(size).fill(ctc.defaultValue),
    // defaultValue: {ty, val: Array(size).fill(ctc.defaultValue)},
  };
};
// Note: curried
/** @example BV_Array(BT_UInt, 3)([1, 2, 3]) */
export const BV_Array = (ctc: BackendTy<CBR_Val>, size: number) => (val: unknown[]): CBR_Array => {
  return BT_Array(ctc, size).canonicalize(val);
}

export const BT_Tuple = (ctcs: Array<BackendTy<CBR_Val>>): BackendTy<CBR_Tuple> => {
  // TODO: check ctcs for sanity
  // const ty: ReachTy = ['Tuple', ctcs.map((ctc) => ctc.ty)];
  return {
    name: `Tuple(${ctcs.map((ctc) => ` ${ctc.name} `)})`,
    // ty,
    canonicalize: (args: any): CBR_Tuple => {
      if (!Array.isArray(args)) {
        throw Error(`Expected a Tuple, but got ${JSON.stringify(args)}`);
      }
      if (ctcs.length != args.length) {
        throw Error(`Expected tuple of size ${ctcs.length}, but got ${args.length}`);
      }
      const val = args.map((arg, i) => ctcs[i].canonicalize(arg));
      return val;
      // return {ty, val: args.map((arg, i) => ctcs[i].canonicalize(arg))};
    },
    defaultValue: ctcs.map(ctc => ctc.defaultValue),
    // defaultValue: {ty, val: ctcs.map(ctc => ctc.defaultValue)},
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
  // const tys: {[key: string]: ReachTy} = {};
  // for (const k in co) {
  //   tys[k] = co[k].ty;
  // }
  // const ty: ReachTy = ['Object', tys];
  return {
    name: `Object(${Object.keys(co).map((k) => ` ${k}: ${co[k].name} `)})`,
    // ty,
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
      // return {ty, val: obj};
    },
    defaultValue: (() => {
      const obj: {
        [key: string]: CBR_Val
      } = {};
      for (const prop in co) {
        obj[prop] = co[prop].defaultValue;
      }
      return obj;
      // return {ty, val: obj};
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
  // const tys: {[key: string]: ReachTy} = {};
  // for (const k in co) {
  //   tys[k] = co[k].ty;
  // }
  // const ty: ReachTy = ['Data', tys];
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
    // ty,
    canonicalize: (io: unknown): CBR_Data => {
      if (!(Array.isArray(io) && io.length == 2 && typeof io[0] == 'string')) {
        throw Error(`Expected an array of length two to represent a data instance, but got ${JSON.stringify(io)}`);
      }
      const vn = io[0];
      if (!{}.hasOwnProperty.call(co, vn)) {
        throw Error(`Expected a variant in ${Object.keys(co)}, but got ${vn}`);
      }
      return [vn, co[vn].canonicalize(io[1])];
      // return {ty, val: [vn, co[vn].canonicalize(io[1])]};
    },
    defaultValue: ((): CBR_Data => {
      const label = ascLabels[0];
      return [label, co[label].defaultValue];
      // return {ty, val: [label, co[label].defaultValue]};
    })(),
  };
};
/** @example BV_Data({x: BT_UInt, y: BT_Bytes})(['x', 3]); */
export const BV_Data = (co: {
  [key: string]: BackendTy<CBR_Val>,
}) => (val: [string, unknown]) => {
  return BT_Data(co).canonicalize(val);
}
