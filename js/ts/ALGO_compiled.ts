// ****************************************************************************
// standard library needed at runtime by compiled Reach programs
// ****************************************************************************

import * as shared from './shared';
import algosdk from 'algosdk';
import ethers from 'ethers';
import {
  CBR_Null,
  CBR_Bool,
  CBR_UInt,
  CBR_Bytes,
  CBR_Address,
  CBR_Digest,
  CBR_Object,
  CBR_Data,
  CBR_Array,
  CBR_Tuple,
  CBR_Val,
} from './CBR';
import * as CBR from './CBR';
import { labelMaps } from './shared_impl';


type BigNumber = ethers.BigNumber;

const BigNumber = ethers.BigNumber;

export const UInt_max: BigNumber =
  BigNumber.from(2).pow(64).sub(1);

// NV = Net Value
export type NV = Uint8Array;

export type ALGO_Ty<BV extends CBR_Val> = {
  name: string,
  canonicalize: (uv: unknown) => BV,
  netSize: number
  toNet(bv: BV): NV,
  fromNet(nv: NV): BV,
}

export const digest = shared.makeDigest((t:ALGO_Ty<any>, v:any) => t.toNet(v));

export const T_Null: ALGO_Ty<CBR_Null> = {
  ...CBR.BT_Null,
  netSize: 0,
  toNet: (bv: CBR_Null): NV => (void(bv), new Uint8Array([])),
  fromNet: (nv: NV): CBR_Null => (void(nv), null),
}

export const T_Bool: ALGO_Ty<CBR_Bool> = {
  ...CBR.BT_Bool,
  netSize: 1,
  toNet: (bv: CBR_Bool): NV => new Uint8Array([bv ? 1 : 0]),
  fromNet: (nv: NV): CBR_Bool => nv[0] == 1,
}

export const T_UInt: ALGO_Ty<CBR_UInt> = {
  ...CBR.BT_UInt,
  netSize: 8, // UInt64
  toNet: (bv: CBR_UInt): NV => (
    ethers.utils.zeroPad(ethers.utils.arrayify(bv), 8)
  ),
  fromNet: (nv: NV): CBR_UInt => {
    // debug(`fromNet: UInt`);
    // if (getDEBUG()) console.log(nv);
    return ethers.BigNumber.from(nv);
  },
}

/** @description For arbitrary utf8 strings */
const stringyNet = {
  toNet: (bv: CBR_Bytes): NV => (
    ethers.utils.toUtf8Bytes(bv)
  ),
  fromNet: (nv: NV): CBR_Bytes => (
    ethers.utils.toUtf8String(nv)
  ),
}

/** @description For hex strings representing bytes */
const bytestringyNet = {
  toNet: (bv: string): NV => (
    ethers.utils.arrayify(bv)
  ),
  fromNet: (nv: NV): string => (
    ethers.utils.hexlify(nv)
  )
};

export const T_Bytes = (len:number): ALGO_Ty<CBR_Bytes> => ({
  ...CBR.BT_Bytes(len),
  ...stringyNet,
  netSize: len,
});

export const T_Digest: ALGO_Ty<CBR_Digest> = {
  ...CBR.BT_Digest,
  ...bytestringyNet,
  netSize: 32,
};

function addressUnwrapper(x: any): string {
  const addr =
    x && x.networkAccount && x.networkAccount.addr
    || x && x.addr;

  return (addr != undefined)
    ? '0x' + Buffer.from(algosdk.decodeAddress(addr).publicKey).toString('hex')
    : x;
}
export const T_Address: ALGO_Ty<CBR_Address> = {
  ...CBR.BT_Address,
  ...bytestringyNet,
  netSize: 32,
  canonicalize: (uv: unknown): CBR_Address => {
    const val = addressUnwrapper(uv);
    return CBR.BT_Address.canonicalize(val || uv)
  }
}

export const T_Array = (
  co: ALGO_Ty<CBR_Val>,
  size: number,
): ALGO_Ty<CBR_Array> => ({
  ...CBR.BT_Array(co, size),
  netSize: size * co.netSize,
  toNet: (bv: CBR_Array): NV => {
    return ethers.utils.concat(bv.map((v) => co.toNet(v)));
  },
  fromNet: (nv: NV): CBR_Array => {
    const chunks = new Array(size).fill(null);
    let rest = nv;
    for (const i in chunks) {
      chunks[i] = co.fromNet(rest.slice(0, co.netSize));
      rest = rest.slice(co.netSize);
    }
    // TODO: assert size of nv/rest is correct?
    return chunks;
  },
});

export const T_Tuple = (
  cos: Array<ALGO_Ty<CBR_Val>>,
): ALGO_Ty<CBR_Tuple> => ({
  ...CBR.BT_Tuple(cos),
  netSize: (
    cos.reduce((acc, co) => acc + co.netSize, 0)
  ),
  toNet: (bv: CBR_Tuple): NV => {
    const val = cos.map((co, i) => co.toNet(bv[i]));
    return ethers.utils.concat(val);
  },
  // TODO: share more code w/ T_Array.fromNet
  fromNet: (nv: NV): CBR_Tuple => {
    const chunks: Array<CBR_Val> = new Array(cos.length).fill(null);
    let rest = nv;
    for (const i in cos) {
      const co = cos[i];
      chunks[i] = co.fromNet(rest.slice(0, co.netSize));
      rest = rest.slice(co.netSize);
    }
    return chunks;
  },
});

export const T_Object = (
  coMap: {[key: string]: ALGO_Ty<CBR_Val>}
): ALGO_Ty<CBR_Object> => {
  const cos = Object.values(coMap);
  const netSize =
    cos.reduce((acc, co) => acc + co.netSize, 0);
  const {ascLabels} = labelMaps(coMap);
  return {
    ...CBR.BT_Object(coMap),
    netSize,
    toNet: (bv: CBR_Object): NV => {
      const chunks = ascLabels.map((label) =>
        coMap[label].toNet(bv[label])
      );
      return ethers.utils.concat(chunks);
    },
    // TODO: share more code w/ T_Array.fromNet and T_Tuple.fromNet
    fromNet: (nv: NV): CBR_Object => {
      const obj: {[key: string]: CBR_Val} = {};
      let rest = nv;
      for (const iStr in ascLabels) {
        const i = parseInt(iStr);
        const label = ascLabels[i];
        const co = coMap[label];
        obj[label] = co.fromNet(rest.slice(0, co.netSize));
        rest = rest.slice(co.netSize);
      }
      return obj;
    },
  }
};

// 1 byte for the label
// the rest right-padded with zeroes
// up to the size of the largest variant
export const T_Data = (
  coMap: {[key: string]: ALGO_Ty<CBR_Val>}
): ALGO_Ty<CBR_Data> => {
  const cos = Object.values(coMap);
  const valSize =
    Math.max(...cos.map((co) => co.netSize))
  const netSize = valSize + 1;
  const {ascLabels, labelMap} = labelMaps(coMap);
  return {
    ...CBR.BT_Data(coMap),
    netSize,
    toNet: ([label, val]: CBR_Data): NV => {
      const i = labelMap[label];
      const lab_nv = new Uint8Array([i]);
      const val_co = coMap[label];
      const val_nv = val_co.toNet(val);
      const padding = new Uint8Array(valSize - val_nv.length);
      return ethers.utils.concat([lab_nv, val_nv, padding]);
    },
    fromNet: (nv: NV): CBR_Data => {
      const i = nv[0];
      const label = ascLabels[i];
      const val_co = coMap[label];
      const rest = nv.slice(1);
      const sliceTo = val_co.netSize;
      const val = val_co.fromNet(rest.slice(0, sliceTo));
      return [label, val];
    },
  }
}

export const addressEq = shared.mkAddressEq(T_Address);

export const typeDefs = {
  T_Null,
  T_Bool,
  T_UInt,
  T_Bytes,
  T_Address,
  T_Digest,
  T_Object,
  T_Data,
  T_Array,
  T_Tuple
};

export const stdlib = {
  ...shared,
  T_Null,
  T_Bool,
  T_UInt,
  T_Bytes,
  T_Address,
  T_Digest,
  T_Object,
  T_Data,
  T_Array,
  T_Tuple,
  addressEq,
  digest,
  UInt_max,
};
