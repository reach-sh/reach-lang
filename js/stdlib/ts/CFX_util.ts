// lightly adapted from @conflux-dev/conflux-address-js@1.0.0
import JSBI from 'jsbi';
import type { EpochNumber } from '@reach-sh/js-conflux-sdk';
import { debug } from './shared_impl';

const ALPHABET = 'ABCDEFGHJKMNPRSTUVWXYZ0123456789'
const ALPHABET_MAP: {[key: string]: number} = {}
for (let z = 0; z < ALPHABET.length; z++) {
  const x = ALPHABET.charAt(z)
  if (ALPHABET_MAP[x] !== undefined) {
    throw new TypeError(x + ' is ambiguous')
  }
  ALPHABET_MAP[x] = z
}

// pre defined BigInt could faster about 40 percent
const BIGINT_0 = JSBI.BigInt(0)
const BIGINT_1 = JSBI.BigInt(1)
const BIGINT_5 = JSBI.BigInt(5)
const BIGINT_35 = JSBI.BigInt(35)
const BIGINT_0B00001 = JSBI.BigInt(0b00001)
const BIGINT_0B00010 = JSBI.BigInt(0b00010)
const BIGINT_0B00100 = JSBI.BigInt(0b00100)
const BIGINT_0B01000 = JSBI.BigInt(0b01000)
const BIGINT_0B10000 = JSBI.BigInt(0b10000)
const BIGINT_0X07FFFFFFFF = JSBI.BigInt(0x07ffffffff)
const BIGINT_0X98F2BC8E61 = JSBI.BigInt(0x98f2bc8e61)
const BIGINT_0X79B76D99E2 = JSBI.BigInt(0x79b76d99e2)
const BIGINT_0XF33E5FB3C4 = JSBI.BigInt(0xf33e5fb3c4)
const BIGINT_0XAE2EABE2A8 = JSBI.BigInt(0xae2eabe2a8)
const BIGINT_0X1E4F43E470 = JSBI.BigInt(0x1e4f43e470)

function convertBit (buffer: number[], inBits: number, outBits: number, pad: boolean = false): number[] {
  const mask = (1 << outBits) - 1
  const array = []

  let bits = 0
  let value = 0
  for (const byte of buffer) {
    bits += inBits
    value = (value << inBits) | byte

    while (bits >= outBits) {
      bits -= outBits
      array.push((value >>> bits) & mask)
    }
  }
  value = (value << (outBits - bits)) & mask

  if (bits && pad) {
    array.push(value)
  } else if (value && !pad) {
    throw new Error('Excess padding')
  } else if (bits >= inBits && !pad) {
    throw new Error('Non-zero padding')
  }

  return array
}

function polyMod (buffer: number[]): JSBI {
  let checksumBigInt = BIGINT_1
  for (const byte of buffer) {
    // c0 = c >> 35;
    const high = JSBI.signedRightShift(checksumBigInt, BIGINT_35) // XXX: checksumBigInt must be positive, signedRightShift is ok

    // c = ((c & 0x07ffffffff) << 5) ^ d;
    checksumBigInt = JSBI.bitwiseAnd(checksumBigInt, BIGINT_0X07FFFFFFFF)
    checksumBigInt = JSBI.leftShift(checksumBigInt, BIGINT_5)
    checksumBigInt = byte ? JSBI.bitwiseXor(checksumBigInt, JSBI.BigInt(byte)) : checksumBigInt // bit ^ 0 = bit

    if (JSBI.notEqual(JSBI.bitwiseAnd(high, BIGINT_0B00001), BIGINT_0)) {
      checksumBigInt = JSBI.bitwiseXor(checksumBigInt, BIGINT_0X98F2BC8E61)
    }
    if (JSBI.notEqual(JSBI.bitwiseAnd(high, BIGINT_0B00010), BIGINT_0)) {
      checksumBigInt = JSBI.bitwiseXor(checksumBigInt, BIGINT_0X79B76D99E2)
    }
    if (JSBI.notEqual(JSBI.bitwiseAnd(high, BIGINT_0B00100), BIGINT_0)) {
      checksumBigInt = JSBI.bitwiseXor(checksumBigInt, BIGINT_0XF33E5FB3C4)
    }
    if (JSBI.notEqual(JSBI.bitwiseAnd(high, BIGINT_0B01000), BIGINT_0)) {
      checksumBigInt = JSBI.bitwiseXor(checksumBigInt, BIGINT_0XAE2EABE2A8)
    }
    if (JSBI.notEqual(JSBI.bitwiseAnd(high, BIGINT_0B10000), BIGINT_0)) {
      checksumBigInt = JSBI.bitwiseXor(checksumBigInt, BIGINT_0X1E4F43E470)
    }
  }

  return JSBI.bitwiseXor(checksumBigInt, BIGINT_1)
}

// lightly adapted from @conflux-dev/conflux-address-js@1.0.0
const VERSION_BYTE = 0
const NET_ID_LIMIT = 0xFFFFFFFF

function encodeNetId (netId:number) {
  if (!Number.isInteger(netId)) {
    throw new Error('netId should be passed as an integer')
  }
  if (netId <= 0 || netId > NET_ID_LIMIT) {
    throw new Error('netId should be passed as in range [1, 0xFFFFFFFF]')
  }

  switch (netId) {
    case 1:
      return 'cfxtest'
    case 1029:
      return 'cfx'
    default:
      return `net${netId}`
  }
}

function isValidNetId (netId:string) {
  return /^([1-9]\d*)$/.test(netId) && Number(netId) <= NET_ID_LIMIT
}

function decodeNetId (payload:string) {
  switch (payload) {
    case 'cfxtest':
      return 1
    case 'cfx':
      return 1029
    default: {
      const prefix = payload.slice(0, 3)
      const netId = payload.slice(3)
      if (prefix !== 'net' || !isValidNetId(netId)) {
        throw new Error("netId prefix should be passed by 'cfx', 'cfxtest' or 'net[n]' ")
      }
      if (Number(netId) === 1 || Number(netId) === 1029) {
        throw new Error('net1 or net1029 are invalid')
      }
      return Number(netId)
    }
  }
}

function getAddressType (hexAddress:Buffer) {
  if (hexAddress.length < 1) {
    throw new Error('Empty payload in address')
  }

  switch (hexAddress[0] & 0xf0) {
    case 0x10:
      return 'user'
    case 0x80:
      return 'contract'
    case 0x00:
      for (const x of hexAddress) {
        if (x !== 0x00) {
          return 'builtin'
        }
      }
      return 'null'
    default:
      throw new Error('hexAddress should start with 0x0, 0x1 or 0x8')
  }
}

// mimicking cfxsdk.address.encodeCfxAddress
export function encodeCfxAddress(ha_in: Buffer|Uint8Array, netId: number): string {
  const hexAddress: Buffer = (ha_in instanceof Buffer) ? ha_in : Buffer.from(ha_in);

  if (hexAddress.length < 20) {
    throw new Error('hexAddress should be at least 20 bytes')
  }

  const addressType = getAddressType(hexAddress).toUpperCase()
  const netName = encodeNetId(netId).toUpperCase()

  const netName5Bits = Buffer.from(netName).map(byte => byte & 0b11111)
  const payload5Bits = convertBit([VERSION_BYTE, ...hexAddress], 8, 5, true)

  const checksumBigInt = polyMod([...netName5Bits, 0, ...payload5Bits, 0, 0, 0, 0, 0, 0, 0, 0])
  const checksumBytes = [ ...Buffer.from(checksumBigInt.toString(16).padStart(10, '0'), 'hex') ]
  const checksum5Bits = convertBit(checksumBytes, 8, 5, true)

  const payload = payload5Bits.map(byte => ALPHABET[byte]).join('')
  const checksum = checksum5Bits.map(byte => ALPHABET[byte]).join('')

  return `${netName}:TYPE.${addressType}:${payload}${checksum}`;
};

// mimicking cfxsdk.address.decodeCfxAddress
export function decodeCfxAddress(address: string): {hexAddress: Buffer, netId: number, type: string} {
  debug(`decode`, {address});
  // don't allow mixed case
  const lowered = address.toLowerCase()
  const uppered = address.toUpperCase()
  if (address !== lowered && address !== uppered) {
    throw new Error('Mixed-case address ' + address)
  }

  const am = address.toUpperCase().match(/^([^:]+):(.+:)?(.{34})(.{8})$/);
  if ( ! am ) { throw Error(`Invalid address: ${address}`); }
  const [, netName, shouldHaveType, payload, checksum] = am;

  const prefix5Bits = [ ...Buffer.from(netName) ].map((byte:number) => byte & 0b11111)
  const payload5Bits = []
  for (const char of payload) {
    payload5Bits.push(ALPHABET_MAP[char])
  }
  const checksum5Bits = []
  for (const char of checksum) {
    checksum5Bits.push(ALPHABET_MAP[char])
  }

  const [version, ...addressBytes] = convertBit(payload5Bits, 5, 8)
  if (version !== VERSION_BYTE) {
    throw new Error('Can not recognize version byte')
  }

  const hexAddress = Buffer.from(addressBytes)
  const netId = decodeNetId(netName.toLowerCase())
  const type = getAddressType(hexAddress)

  if (shouldHaveType) {
    const actual = `type.${type}:`;
    const expected = shouldHaveType.toLowerCase();
    if ( actual !== expected ) {
      throw new Error(`Type of address doesn't match, got '${actual}', expected '${expected}'`);
    }
  }

  const bigInt = polyMod([...prefix5Bits, 0, ...payload5Bits, ...checksum5Bits])
  if (Number(bigInt)) {
    throw new Error(`Invalid checksum for ${address}`)
  }

  return { hexAddress, netId, type };
};

// TODO: 'latest_state' seems to work well; is there a better choice?
export const defaultEpochTag: EpochNumber = 'latest_state';

/** @description Precondition: addrC is a valid Conflux address */
export function address_cfxStandardize(addrC: string): string {
  debug(`address_cfxStandardize`, {addrC});
  const pieces = addrC.split(':');
  //debug(`address_cfxStandardize`, pieces.length, {pieces});
  if (pieces.length === 3) {
    addrC = `${pieces[0]}:${pieces[2]}`;
  } else if (pieces.length !== 2) {
    throw Error(`impossible: bad CFX addr: '${addrC}'`);
  }
  //debug(`address_cfxStandardize`, {addrC});
  return addrC.toUpperCase();
}

