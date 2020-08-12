import * as nodeAssert from 'assert';
import * as crypto     from 'crypto';
import ethers          from 'ethers';

const un0x           = h => h.replace(/^0x/, '');
const hexTo0x        = h => '0x' + h.replace(/^0x/, '');
const byteToHex      = b => (b & 0xFF).toString(16).padStart(2, '0');
const byteArrayToHex = b => Array.from(b, byteToHex).join('');

const hexOf = x =>
      typeof x === 'string' && x.slice(0, 2) === '0x'
      ? un0x(toHex(x))
      : un0x(toHex(`0x${x}`));

export const T_Null = (v) => v == null;

export const T_Bool = (v) => typeof(v) === 'boolean';

export const T_UInt256 = (v) => isBN(v);

export const T_Bytes = (x) => typeof(x) === 'string';

export const T_Address = (x) => isHex(x) || typeof(x) === 'string';

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
  else { throw Error(`Expected ${how}, got: "${what}"`); } };

export const assert = d => nodeAssert.strict(d);

// XXX export the ethers names for these things?
const {
  BigNumber,
  bigNumberify,
  hexlify,
  toUtf8Bytes,
  toUtf8String,
  isHexString,
} = ethers.utils;
const { isBigNumber } = BigNumber;

export const toBN = bigNumberify;
export const isBN = isBigNumber;

// Massage the arg into a form keccak256 will handle correctly
const kek = (arg) => {
  if (typeof(arg) === 'string') {
    if (isHex(arg)) {
      return arg;
    } else {
      return toUtf8Bytes(arg);
    }
  } else if (typeof(arg) === 'number') {
    return '0x' + bnToHex(arg);
  } else if (isBN(arg)) {
    return '0x' + bnToHex(arg);
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

export const hexToBN = h => toBN(hexTo0x(h));
export const uint256_to_bytes = i => bnToHex(i);

// size is in bytes; default size 32 = 256 bytes
export const bnToHex = (u, size = 32) => {
  const nPos = bigNumberify(u).toTwos(8 * size);
  const nArr = ethers.utils.padZeros(nPos, size);
  // XXX why do we slice off the 0x?
  return hexlify(nArr).slice(2);
};

export const bytes_eq = (x, y) =>
  hexOf(x) === hexOf(y);

const random_uint256 = () =>
  hexToBN(byteArrayToHex(crypto.randomBytes(32)));

export const hasRandom = {
  random: random_uint256 };

export const eq    = (a, b) => toBN(a).eq( toBN(b));
export const add   = (a, b) => toBN(a).add(toBN(b));
export const sub   = (a, b) => toBN(a).sub(toBN(b));
export const mod   = (a, b) => toBN(a).mod(toBN(b));
export const mul   = (a, b) => toBN(a).mul(toBN(b));
export const div   = (a, b) => toBN(a).div(toBN(b));
export const ge    = (a, b) => toBN(a).gte(toBN(b));
export const gt    = (a, b) => toBN(a).gt( toBN(b));
export const le    = (a, b) => toBN(a).lte(toBN(b));
export const lt    = (a, b) => toBN(a).lt( toBN(b));
