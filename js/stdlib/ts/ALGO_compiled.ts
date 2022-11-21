import { Stdlib_Backend_Base } from './interfaces';
import * as shared_backend from './shared_backend';
import {
  debug,
  labelMaps,
  makeDigest,
  mkAddressEq,
  MkPayAmt,
  makeArith,
  UInt256_max,
  canonicalToBytes,
} from './shared_impl';
import {
  bigNumberToNumber,
  bigNumberify,
  bigNumberToBigInt,
} from './shared_user';
import {
  defineSimStuff,
} from './shared_sim';
import algosdk from 'algosdk';
import buffer from 'buffer';
import { ethers } from 'ethers';
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
  CBR_Struct,
  CBR_Val,
  BackendTy,
} from './CBR';
import * as CBR from './CBR';

type BigNumber = ethers.BigNumber;

const BigNumber = ethers.BigNumber;
const Buffer = buffer.Buffer;

export const UInt_max: BigNumber =
  BigNumber.from(2).pow(64).sub(1);

// NV = Net Value
export type NV = Uint8Array;

export interface ALGO_Ty<BV extends CBR_Val> extends BackendTy<BV> {
  netSize: number
  toNet(bv: BV): NV,
  fromNet(nv: NV): BV,
  netName: string,
};

const instAlgoTy = (ty: any) =>
  ({ ...ty, toString: () => ty.netName });

export const digest =
  makeDigest('sha256', (ts:any[], vs:any[]) => T_Tuple(ts).toNet(vs));

export const T_Null: ALGO_Ty<CBR_Null> = instAlgoTy({
  ...CBR.BT_Null,
  netSize: 0,
  toNet: (bv: CBR_Null): NV => (void(bv), new Uint8Array([])),
  fromNet: (nv: NV): CBR_Null => (void(nv), null),
  netName: 'byte[0]',
})

export const T_Bool: ALGO_Ty<CBR_Bool> = instAlgoTy({
  ...CBR.BT_Bool,
  netSize: 1,
  toNet: (bv: CBR_Bool): NV => new Uint8Array([bv ? 1 : 0]),
  fromNet: (nv: NV): CBR_Bool => nv[0] == 1,
  netName: 'byte', // XXX 'bool'
})

export const T_UInt: ALGO_Ty<CBR_UInt> = instAlgoTy({
  ...CBR.BT_UInt(UInt_max),
  netSize: 8, // UInt64
  toNet: (bv: CBR_UInt): NV => {
    try {
      return ethers.utils.zeroPad(ethers.utils.arrayify(bv), 8)
    } catch (e) {
      throw new Error(`toNet: ${bv} is out of range [0, ${UInt_max}]`);
    }
  },
  fromNet: (nv: NV): CBR_UInt => {
    // debug(`fromNet: UInt`, nv);
    // if (getDEBUG()) console.log(nv);
    return ethers.BigNumber.from(nv.slice(0, 8));
  },
  netName: 'uint64',
})

export const T_UInt256: ALGO_Ty<CBR_UInt> = instAlgoTy({
  ...CBR.BT_UInt(UInt256_max),
  netSize: 32, // UInt
  toNet: (bv: CBR_UInt): NV => {
    try {
      return ethers.utils.zeroPad(ethers.utils.arrayify(bv), 32)
    } catch (e) {
      throw new Error(`toNet: ${bv} is out of range [0, ${UInt256_max}]`);
    }
  },
  fromNet: (nv: NV): CBR_UInt => {
    // debug(`fromNet: UInt`, nv);
    // if (getDEBUG()) console.log(nv);
    return ethers.BigNumber.from(nv.slice(0, 32));
  },
  netName: 'uint256',
})

/** @description For arbitrary utf8 strings */
const stringyNet = (len:number) => ({
  toNet: canonicalToBytes,
  fromNet: (nv: NV): CBR_Bytes => {
    const nvp = nv.slice(0, len);
    // XXX temporary while we do not distingiush between raw bytes / utf8 strings.
    try { return ethers.utils.toUtf8String(nvp); }
    catch (_) { return ethers.utils.arrayify(nvp); }
  },
});

/** @description For hex strings representing bytes */
export const bytestringyNet = (len:number) => ({
  netSize: len,
  netName: `byte[${len}]`,
  toNet: (bv: string): NV => {
    return ethers.utils.arrayify(bv);
  },
  fromNet: (nv: NV): string => {
    return ethers.utils.hexlify(nv.slice(0, len));
  }
});

export const T_Bytes = (len:number): ALGO_Ty<CBR_Bytes> => instAlgoTy({
  ...CBR.BT_Bytes(len),
  ...stringyNet(len),
  netSize: bigNumberToNumber(len),
  netName: `byte[${len}]`,
});

export const T_BytesDyn: ALGO_Ty<CBR_Bytes> = instAlgoTy({
  ...CBR.BT_BytesDyn,
  toNet: canonicalToBytes,
  fromNet: (nv: NV): CBR_Bytes => (
    ethers.utils.toUtf8String(nv)
  ),
  netSize: 32, // XXX
  netName: `byte[]`,
});

export const T_StringDyn: ALGO_Ty<CBR_Bytes> = instAlgoTy({
  ...CBR.BT_StringDyn,
  toNet: canonicalToBytes,
  fromNet: (nv: NV): CBR_Bytes => (
    ethers.utils.toUtf8String(nv)
  ),
  netSize: 32, // XXX
  netName: `string`,
});

export const T_Digest: ALGO_Ty<CBR_Digest> = instAlgoTy({
  ...CBR.BT_Digest,
  ...bytestringyNet(32),
  netName: `digest`,
});

export const addressToHex = (x: string): string =>
  '0x' + Buffer.from(algosdk.decodeAddress(x).publicKey).toString('hex');

export const addressFromHex = (hexAddr: string): string =>
  algosdk.encodeAddress(Buffer.from(hexAddr.slice(2), 'hex'));

const extractAddrM = (x: any): string|false => {
  const addr: string|false =
    (x && x.networkAccount && x.networkAccount.addr)
    || (x && x.addr)
    || (typeof x === 'string' && x);
  //debug(`extractAddrM`, {x, addr});
  return addr;
};

export const extractAddr = (x:any): string => {
  const a = extractAddrM(x);
  //debug(`extractAddr`, {x, a});
  if ( a === false ) { throw Error(`Expected address, got ${x}`); }
  return a;
};

function addressUnwrapper(x: any): string {
  const addr = extractAddrM(x);
  return !addr ? x
   : addr.slice(0, 2) === '0x' ? addr
   : addressToHex(addr);
};

export const T_Address: ALGO_Ty<CBR_Address> = instAlgoTy({
  ...CBR.BT_Address,
  ...bytestringyNet(32),
  netSize: 32,
  canonicalize: (uv: unknown): CBR_Address => {
    const val = addressUnwrapper(uv);
    const hs = CBR.BT_Address.canonicalize(val || uv);
    // We are filling up with zeros if the address is less than 32 bytes
    return hs.padEnd(32*2+2, '0');
  },
  netName: `address`,
});

export const T_Contract: ALGO_Ty<Contract> = instAlgoTy({
  ...T_UInt,
  name: 'Contract',
});

export const T_Array = (
  co: ALGO_Ty<CBR_Val>,
  size_u: unknown,
): ALGO_Ty<CBR_Array> => {
  const size = bigNumberToNumber(bigNumberify(size_u));
  debug('T_Array', co, size);
  const asTuple = T_Tuple(new Array(size).fill(co));
  debug('T_Array', asTuple);
  const { netSize, toNet, fromNet } = asTuple;
  return instAlgoTy({
    ...CBR.BT_Array(co, size),
    netSize, toNet, fromNet,
    netName: `${co.netName}[${size}]`,
  });
};

export const T_Tuple = (
  cos: Array<ALGO_Ty<CBR_Val>>,
): ALGO_Ty<CBR_Tuple> => instAlgoTy({
  ...CBR.BT_Tuple(cos),
  netSize: (
    cos.reduce(((acc:number, co:ALGO_Ty<CBR_Val>): number =>
                  acc + co.netSize),
               0)
  ),
  toNet: (bv: CBR_Tuple): NV => {
    const val = cos.map((co, i) => co.toNet(bv[i]));
    return ethers.utils.concat(val);
  },
  // TODO: share more code w/ T_Array.fromNet
  fromNet: (nv: NV): CBR_Tuple => {
    //debug(`Tuple.fromNet`, cos.map((x) => x.name), nv);
    const chunks: Array<CBR_Val> = new Array(cos.length).fill(null);
    let rest = nv;
    for (const i in cos) {
      const co = cos[i];
      chunks[i] = co.fromNet(rest.slice(0, co.netSize));
      rest = rest.slice(co.netSize);
    }
    return chunks;
  },
  netName: `(${cos.map(c => c.netName).join(',')})`,
});

export const T_Struct = (
  cos: Array<[string, ALGO_Ty<CBR_Val>]>,
): ALGO_Ty<CBR_Struct> => instAlgoTy({
  ...CBR.BT_Struct(cos),
  netSize: (
    cos.reduce((acc, co) => acc + co[1].netSize, 0)
  ),
  toNet: (bv: CBR_Struct): NV => {
    const val = cos.map(([k, co]) => co.toNet(bv[k]));
    return ethers.utils.concat(val);
  },
  // TODO: share more code w/ T_Array.fromNet
  fromNet: (nv: NV): CBR_Struct => {
    const obj: any = {};
    let rest = nv;
    for (const i in cos) {
      const [k, co] = cos[i];
      obj[k] = co.fromNet(rest.slice(0, co.netSize));
      rest = rest.slice(co.netSize);
    }
    return obj;
  },
  netName: `(${cos.map(c => c[1].netName).join(',')})`,
});

export const T_Object = (
  coMap: {[key: string]: ALGO_Ty<CBR_Val>}
): ALGO_Ty<CBR_Object> => {
  const cos = Object.values(coMap);
  const netSize =
    cos.reduce((acc, co) => acc + co.netSize, 0);
  const {ascLabels} = labelMaps(coMap);
  return instAlgoTy({
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
    netName: `(${cos.map(c => c.netName).join(',')})`,
  })
};

// 1 byte for the label
// the rest right-padded with zeroes
// up to the size of the largest variant
export const T_Data = (
  coMap: {[key: string]: ALGO_Ty<CBR_Val>}
): ALGO_Ty<CBR_Data> => {
  const cos = Object.values(coMap);
  const cosSizes = cos.map((co) => co.netSize);
  const valSize = Math.max(...cosSizes);
  const netSize = valSize + 1;
  debug(`T_Data`, { cos, cosSizes, valSize, netSize });
  const {ascLabels, labelMap} = labelMaps(coMap);
  return instAlgoTy({
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
      debug({nv, i, label, val_co});
      const rest = nv.slice(1);
      const sliceTo = val_co.netSize;
      const val = val_co.fromNet(rest.slice(0, sliceTo));
      return [label, val];
    },
    netName: `(byte,byte[${valSize}])`,
  })
}

export const addressEq = mkAddressEq(T_Address);
export const digestEq = shared_backend.bytesEq;
export const digest_xor = shared_backend.digest_xor;
export const bytes_xor = shared_backend.bytes_xor;
export const btoiLast8 = shared_backend.btoiLast8;

const T_Token = T_UInt;

export type Token = CBR_UInt;

export type Contract = CBR_UInt;
export const ctcAddrEq = (x:unknown, y:unknown): boolean => {
  debug('ctcAddrEq', {x, y});
  const ctc_x = T_Contract.canonicalize(x);
  const addr_y = T_Address.canonicalize(y);
  const addr_x = algosdk.getApplicationAddress(bigNumberToBigInt(ctc_x));
  debug('ctcAddrEq', {addr_x, addr_y});
  return addressEq(addr_x, addr_y);
};

export const tokenEq = (x: unknown, y: unknown): boolean =>
  T_Token.canonicalize(x).eq(T_Token.canonicalize(y));
export type PayAmt = MkPayAmt<Token>;

export const typeDefs = {
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

export const emptyContractInfo = 0;

const arith = makeArith(UInt_max);

type ConnectorTy = ALGO_Ty<any>;
type ContractInfo = Contract;

export const stdlib: Stdlib_Backend_Base<Token, ContractInfo, ConnectorTy> = {
  ...shared_backend,
  ...defineSimStuff<Token, ContractInfo, ConnectorTy>(),
  ...arith,
  ...typeDefs,
  addressEq,
  ctcAddrEq,
  digestEq,
  tokenEq,
  digest,
  UInt_max,
  emptyContractInfo,
};
