import * as nodeAssert from 'assert';
import * as crypto     from 'crypto';
import ethers          from 'ethers';

// Hex helpers
const un0x           = h => h.replace(/^0x/, '');
const hexTo0x        = h => '0x' + h.replace(/^0x/, '');
const byteToHex      = b => (b & 0xFF).toString(16).padStart(2, '0');
const byteArrayToHex = b => Array.from(b, byteToHex).join('');

const hexOf = x =>
      typeof x === 'string' && x.slice(0, 2) === '0x'
      ? un0x(toHex(x))
      : un0x(toHex(`0x${x}`));

// Contracts

export const T_Null = (v) => v == null;

export const T_Bool = (v) => typeof(v) === 'boolean';

export const T_UInt256 = (v) =>
  isBigNumber(v) || typeof(v) === 'number';

export const T_Bytes = (x) => {
  if (typeof(x) === 'string') {
    if (isHex(x)) {
      return true;
    } else {
      throw Error(`Please use toHex on string sent to Reach: "${x}"`);
    }
  } else {
    return false;
  }
};

export const T_Address = (x) => isHex(x); // || typeof(x) === 'string';

export const T_Array = (ctc, sz) => (args) => {
  if (sz != args.length) { return false; }
  for ( let i = 0; i < sz; i++ ) {
    if ( ! ctc(args[i]) ) { return false; } }
  return true; };

export const T_Tuple = (ctcs) => (args) => {
  if (ctcs.length != args.length) { return false; }
  for ( let i = 0; i < ctcs.length; i++ ) {
    if ( ! ctcs[i](args[i]) ) { return false; } }
  return true; };

export const T_Object = (co) => (vo) => {
  for ( const prop in co ) {
    if ( ! co[prop](vo[prop]) ) { return false; } }
  return true; };

export const protect = (how, what) => {
  if ( how(what) ) { return what; }
  else {
    const hows = JSON.stringify(how);
    const whats = JSON.stringify(what);
    throw Error(`Expected ${hows}, got: "${whats}"`); } };

export const assert = d => nodeAssert.strict(d);

const {
  hexlify,
  toUtf8Bytes,
  toUtf8String,
  isHexString,
} = ethers.utils;
const { BigNumber } = ethers;
export const { isBigNumber } = BigNumber;
export const bigNumberify = (x) => BigNumber.from(x);


// Massage the arg into a form keccak256 will handle correctly
const kek = (arg) => {
  if (typeof(arg) === 'string') {
    if (isHex(arg)) {
      return arg;
    } else {
      return toUtf8Bytes(arg);
    }
  } else if (typeof(arg) === 'number') {
    return '0x' + bigNumberToHex(arg);
  } else if (isBigNumber(arg)) {
    return '0x' + bigNumberToHex(arg);
  } else if (arg && arg.constructor && arg.constructor.name == 'Uint8Array'){
    return arg;
  } else {
    throw Error(`Can't kek this: ${arg}`);
  }
};

export const toHex = (x) => hexlify(kek(x));
export const isHex = isHexString;
export const hexToString = toUtf8String;

export const keccak256 = (...args) => {
  const kekArgs = args.map(kek);
  const kekArrs = kekArgs.map(ethers.utils.arrayify);
  const kekCat = ethers.utils.concat(kekArrs);
  return ethers.utils.keccak256(kekCat);
};

export const hexToBigNumber = h => bigNumberify(hexTo0x(h));
export const uint256_to_bytes = i => bigNumberToHex(i);

export const bigNumberToHex = (u) => {
  const size = 32; // bytes // TODO: support other sizes?
  const format = 'ufixed256x0';
  const nPos = bigNumberify(u).toTwos(8 * size);
  // They took away padZeros so we have to use FixedNumber
  const nFix = ethers.FixedNumber.from(nPos.toString(), format);
  // XXX why do we slice off the 0x?
  return hexlify(nFix).slice(2);
};

export const bytes_eq = (x, y) =>
  hexOf(x) === hexOf(y);

export const random_uint256 = () =>
  hexToBigNumber(byteArrayToHex(crypto.randomBytes(32)));

export const hasRandom = {
  random: random_uint256 };

export const eq    = (a, b) => bigNumberify(a).eq( bigNumberify(b));
export const add   = (a, b) => bigNumberify(a).add(bigNumberify(b));
export const sub   = (a, b) => bigNumberify(a).sub(bigNumberify(b));
export const mod   = (a, b) => bigNumberify(a).mod(bigNumberify(b));
export const mul   = (a, b) => bigNumberify(a).mul(bigNumberify(b));
export const div   = (a, b) => bigNumberify(a).div(bigNumberify(b));
export const ge    = (a, b) => bigNumberify(a).gte(bigNumberify(b));
export const gt    = (a, b) => bigNumberify(a).gt( bigNumberify(b));
export const le    = (a, b) => bigNumberify(a).lte(bigNumberify(b));
export const lt    = (a, b) => bigNumberify(a).lt( bigNumberify(b));

// Array helpers

export function array_set(arr, idx, elem) {
  const arrp = arr.slice();
  arrp[idx] = elem;
  return arrp; }
