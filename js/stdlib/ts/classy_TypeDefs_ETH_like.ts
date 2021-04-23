import ethers from 'ethers';
import * as shared from './shared';
import {
  TypeDef,
  TypeDefs,
  A_T_Null,
  A_T_Bool,
  A_T_UInt,
  A_T_Bytes,
  A_T_Digest,
  A_T_Address,
  A_T_Token,
  A_T_Array,
  A_T_Tuple,
  A_T_Struct,
  A_T_Object,
  A_T_Data,
} from './classy_TypeDefs';
import { labelMaps } from './shared_impl';

type BigNumber = ethers.BigNumber;
const BigNumber = ethers.BigNumber;

export abstract class ETH_TypeDef<BV, NV> extends TypeDef {
  abstract readonly paramType: string
  abstract readonly defaultValue: BV
  abstract canonicalize(uv: unknown): BV
  abstract munge(bv: BV): NV
  abstract unmunge(uv: unknown): BV
}

export class T_Null extends A_T_Null implements ETH_TypeDef<null, false> {
  paramType = 'bool'
  defaultValue = null
  munge(bv: null): false {
    if (bv !== null) throw Error(`impossible: expected BV of null to be null, but got ${bv}`);
    return false;
  }
  unmunge(uv: unknown): null {
    if (uv !== false) throw Error(`impossible: expected NV of null to be false, but got ${uv}`);
    return null;
  }
}

export class T_Bool extends A_T_Bool implements ETH_TypeDef<boolean, boolean> {
  paramType = 'bool'
  defaultValue = false
  munge(bv: boolean): boolean {
    if (typeof bv !== 'boolean') throw Error(`impossible: expected BV of boolean to be boolean, but got ${bv}`);
    return bv;
  }
  unmunge(uv: unknown): boolean {
    if (typeof uv !== 'boolean') throw Error(`impossible: expected NV of boolean to be boolean, but got ${uv}`);
    return uv;
  }
}

export class T_UInt extends A_T_UInt implements ETH_TypeDef<BigNumber, BigNumber> {
  width = 32
  paramType = 'uint256'
  defaultValue = ethers.BigNumber.from(0)
  munge(bv: BigNumber): BigNumber {
    if (!bv._isBigNumber) throw Error(`impossible: expected BV of UInt to be BigNumber, but got ${bv}`);
    return bv;
  }
  unmunge(uv: unknown) {
    return this.canonicalize(uv);
  }
}

export class T_Bytes extends A_T_Bytes implements ETH_TypeDef<string, number[]> {
  private readonly _paramType: string
  private readonly _defaultValue: string
  get paramType() { return this._paramType }
  get defaultValue() { return this._defaultValue }
  constructor(len: number) {
    super(len);
    this._paramType = `uint8[${len}]`;
    this._defaultValue = ''.padEnd(len, '\0');
  }
  munge(bv: string): number[] {
    return Array.from(ethers.utils.toUtf8Bytes(bv));
  }
  unmunge(uv: unknown): string {
    // TODO: check that it is what we think it is
    const nv = uv as number[];
    return this.canonicalize(shared.hexToString(ethers.utils.hexlify(nv)));
  }
}

export class T_Digest extends A_T_Digest implements ETH_TypeDef<string, BigNumber> {
  paramType =  'uint256'
  defaultValue = ethers.utils.keccak256([])
  munge(bv: string): BigNumber {
    if (typeof bv !== 'string') throw Error(`impossible: expected BV of digest to be string, but got ${bv}`);
    return BigNumber.from(bv)
  }
  unmunge(uv: unknown): string {
    if (!BigNumber.isBigNumber(uv)) throw Error(`impossible: expected NV of digest to be BigNumber, but got ${uv}`);
    return uv.toHexString()
  }
}

export function unwrapAddress(addr: unknown): string {
  if (typeof addr === 'string') {
    return addr;
  }
  // TODO: check more vigorously so that 'as' cast is not needed
  const acc = addr as {networkAccount?: {address?: string}, address?: string};
  if (acc.networkAccount && acc.networkAccount.address) {
    return (acc.networkAccount.address);
  } else if (acc.address) {
    return acc.address;
  } else {
    throw Error(`Failed to unwrap address ${addr}`);
  }
}

function hexyAddress(addr: unknown): string {
  if (typeof addr !== 'string') throw Error(`impossible: addr not string: ${addr}`)
  return (addr.slice(0, 2) === '0x') ? addr : '0x' + addr;
}

function assertHexyAddress(val: unknown, len: number) {
  if (typeof val !== 'string') {
    throw Error(`Address must be a string, but got: ${JSON.stringify(val)}`);
  } else if (val.slice(0, 2) !== '0x') {
    throw Error(`Address must start with 0x, but got: ${JSON.stringify(val)}`);
  } else if (!ethers.utils.isHexString(val)) {
    throw Error(`Address must be a valid hex string, but got: ${JSON.stringify(val)}`);
  } else if (val.length !== len) {
    throw Error(`Address hex string should be length ${len}, but got ${val.length}`);
  }
}

export class ETH_T_Address extends A_T_Address implements ETH_TypeDef<string, string> {
  paramType = 'address'
  defaultValue = '0x' + Array(40).fill('0').join('')
  canonicalize(uv: unknown): string {
    const unwrapped = unwrapAddress(uv) || uv;
    const val = hexyAddress(unwrapped);
    assertHexyAddress(val, 42);
    return val;
  }
  munge(bv: string): string {
    if (typeof bv !== 'string') throw Error(`impossible: expected BV of address to be string, but got ${bv}`);
    return bv;
  }
  unmunge(uv: unknown): string {
    return this.canonicalize(uv);
  }
}

export class ETH_T_Token extends ETH_T_Address implements A_T_Token {
  // T_Token on ETH is ETH_T_Address in all but name
  name = 'Token'
}

export class T_Array<T> extends A_T_Array implements ETH_TypeDef<T[], unknown[]> {
  private readonly _defaultValue: T[]
  private readonly _paramType: string
  get defaultValue() { return this._defaultValue }
  get paramType() { return this._paramType }
  readonly td: ETH_TypeDef<T, unknown>
  constructor(td: ETH_TypeDef<T, unknown>, size: number) {
    super(td, size);
    this._defaultValue = Array(size).fill(td.defaultValue);
    this._paramType = `${td.paramType}[${size}]`;
    this.td = td;
  }
  // TypeScript, just trust us on this one
  canonicalize(uv: unknown): T[] {
    const ret = super.canonicalize(uv);
    return ret as T[];
  }
  munge(bv: T[]): any {
    if (!Array.isArray(bv)) throw Error(`Expected BV of array to be array, but got ${bv}`);
    const {size, td} = this;
    if ( size == 0 ) {
      return false;
    } else {
      return bv.map((arg: T) => td.munge(arg));
    }
  }
  unmunge(uv: unknown): T[] {
    if (!Array.isArray(uv)) throw Error(`Expected NV of array to be array, but got ${uv}`);
    const {size, td} = this;
    if ( size == 0 ) {
      return [];
    } else {
      return this.canonicalize(uv.map((arg: any) => td.unmunge(arg)));
    }
  }
}

export class T_Tuple extends A_T_Tuple implements ETH_TypeDef<unknown[], unknown[]|false> {
  private readonly _defaultValue: unknown[]
  private readonly _paramType: string
  get defaultValue() { return this._defaultValue }
  get paramType() { return this._paramType }
  readonly tds: ETH_TypeDef<unknown, unknown>[];
  constructor(tds: ETH_TypeDef<unknown, unknown>[]) {
    super(tds);
    this.tds = tds;
    this._defaultValue = tds.map(td => td.defaultValue);
    this._paramType = `tuple(${tds.map((td) => td.paramType).join(',')})`
  }
  munge(bv: unknown[]): unknown[]|false {
    const {tds} = this;
    if (tds.length == 0 ) {
      return false;
    } else {
      return bv.map((arg, i) => tds[i].munge(arg));
    }
  }
  unmunge(uv: unknown) {
    if (!Array.isArray(uv)) throw Error(`Expected NV of tuple to be array, but got ${uv}`);
    const {tds} = this;
    return this.canonicalize(tds.map((td, i) => td.unmunge(uv[i])));
  }
}

export class T_Struct extends A_T_Struct implements ETH_TypeDef<{[key: string]: unknown}, unknown[]|false> {
  private readonly _defaultValue: {[key: string]: unknown}
  private readonly _paramType: string
  get defaultValue() { return this._defaultValue }
  get paramType() { return this._paramType }
  readonly namedTds: [string, ETH_TypeDef<unknown, unknown>][]
  constructor(namedTds: [string, ETH_TypeDef<unknown, unknown>][]) {
    super(namedTds);
    this._defaultValue = {};
    namedTds.forEach(([prop, td]) => {
      this._defaultValue[prop] = td.defaultValue;
    });
    this._paramType = `tuple(${namedTds.map(([k, td]) => { void (k); return td.paramType }).join(',')})`
    this.namedTds = namedTds;
  }
  munge(bv: {[key: string]: unknown}): unknown[]|false {
    const {namedTds} = this;
    if (namedTds.length == 0 ) {
      return false;
    } else {
      return namedTds.map(([k, td]) => td.munge(bv[k]));
    }
  }
  unmunge(uv: unknown) {
    if (!Array.isArray(uv)) throw Error(`Expected NV of tuple to be array, but got ${uv}`);
    const {namedTds} = this;
    return this.canonicalize(namedTds.map(([k, td], i: number) => {
      void (k);
      return td.unmunge(uv[i]);
    }));
  }
}

export class T_Object extends A_T_Object implements ETH_TypeDef<{[key: string]: unknown}, {[key: string]: unknown}|false> {
  private readonly _defaultValue: {[key: string]: unknown}
  private readonly _paramType: string
  get defaultValue() { return this._defaultValue }
  get paramType() { return this._paramType }
  readonly tdMap: {[key: string]: ETH_TypeDef<unknown, unknown>}
  constructor(tdMap: {[key: string]: ETH_TypeDef<unknown, unknown>}) {
    super(tdMap);
    this.tdMap = tdMap;
    this._defaultValue = {};
    for (const prop in tdMap) {
      this._defaultValue[prop] = tdMap[prop].defaultValue;
    }
    const {ascLabels} = labelMaps(tdMap);
    const tupFields = ascLabels.map((label) => `${tdMap[label].paramType} ${label}`).join(',')
    this._paramType = `tuple(${tupFields})`;
  }
  munge(bv: {[key: string]: unknown}): {[key: string]: unknown}|false {
    const {tdMap} = this;
    const obj: {
      [key: string]: any
    } = {};
    let none: boolean = true;
    for (const prop in tdMap) {
      none = false;
      obj[prop] = tdMap[prop].munge(bv[prop]);
    }
    if ( none ) {
      return false;
    } else {
      return obj;
    }
  }
  unmunge(uv: unknown): {[key: string]: unknown} {
    const {tdMap} = this;
    // TODO: more runtime checking that this is a safe cast
    const bv = uv as {[key: string]: unknown};
    const obj: {
      [key: string]: unknown,
    } = {};
    for (const prop in tdMap) {
      obj[prop] = tdMap[prop].unmunge(bv[prop]);
    }
    return this.canonicalize(obj);
  }
}

export class T_Data extends A_T_Data implements ETH_TypeDef<[string, unknown], unknown[]|false> {
  private readonly _defaultValue: [string, unknown]
  private readonly _paramType: string
  get defaultValue() { return this._defaultValue }
  get paramType() { return this._paramType }
  readonly tdMap: {[key: string]: ETH_TypeDef<unknown, unknown>}
  readonly ascLabels: string[]
  readonly labelMap: {[key: string]: number}
  constructor(tdMap: {[key: string]: ETH_TypeDef<unknown, unknown>}) {
    super(tdMap);
    const {ascLabels, labelMap} = labelMaps(tdMap);
    this.tdMap = tdMap;
    this.ascLabels = ascLabels;
    this.labelMap = labelMap;
    const label = ascLabels[0];
    this._defaultValue = [label, tdMap[label].defaultValue];
    // See comment on unmunge about field names that we could use but currently don't
    const optionTys = ascLabels.map((label) => `${tdMap[label].paramType} _${label}`)
    // TODO: parameterize on the T_UInt, rather than assuming it's always this one?
    const tupFields = [`${(new T_UInt()).paramType} which`].concat(optionTys).join(',');
    this._paramType = `tuple(${tupFields})`;
  }
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
  munge([label, v]: [string, unknown]): unknown[] {
    const {labelMap, ascLabels, tdMap} = this;
    const i = labelMap[label];
    const vals = ascLabels.map((label) => {
      const vco = tdMap[label];
      return vco.munge(vco.defaultValue);
    });
    vals[i] = tdMap[label].munge(v);
    const ret = [i as unknown];
    return ret.concat(vals);
  }
  // Note: when it comes back from solidity, vs behaves like an N+1-tuple,
  // but also has secret extra keys you can access,
  // based on the struct field names.
  // e.g. Maybe has keys vs["which"], vs["_None"], and vs["_Some"],
  // corresponding to    vs[0],       vs[1],       and vs[2] respectively.
  // We don't currently use these, but we could.
  unmunge(uv: unknown): [string, unknown] {
    if (!Array.isArray(uv)) throw Error(`impossible: expected NV of Data to be array, but got ${uv}`);
    const {tdMap, ascLabels} = this;
    const i = uv[0] as unknown as number;
    const label = ascLabels[i];
    const val = uv[i + 1];
    return this.canonicalize([label, tdMap[label].unmunge(val)]);
  }
}

export class ETH_TypeDefs extends TypeDefs {
  readonly T_Null = new T_Null()
  readonly T_Bool = new T_Bool()
  readonly T_UInt = new T_UInt()
  readonly T_Digest = new T_Digest()
  readonly T_Address = new ETH_T_Address()
  readonly T_Token = new ETH_T_Token()
  T_Bytes(size: number) { return new T_Bytes(size) }
  T_Array<T>(td: ETH_TypeDef<T, unknown>, size: number) { return new T_Array(td, size) }
  T_Tuple(tds: ETH_TypeDef<unknown, unknown>[]) {return new T_Tuple(tds) }
  T_Struct(namedTds: [string, ETH_TypeDef<unknown, unknown>][]) { return new T_Struct(namedTds) }
  T_Object(tdMap: {[key: string]: ETH_TypeDef<unknown, unknown>}) { return new T_Object(tdMap) }
  T_Data(tdMap: {[key: string]: ETH_TypeDef<unknown, unknown>}) { return new T_Data(tdMap) }
}
