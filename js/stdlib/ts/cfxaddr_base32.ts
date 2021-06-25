// lightly adapted from @conflux-dev/conflux-address-js@1.0.0
// @ts-nocheck

const JSBI = require('jsbi')
const ALPHABET = 'ABCDEFGHJKMNPRSTUVWXYZ0123456789'

const ALPHABET_MAP = {}
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

function convertBit (buffer, inBits, outBits, pad) {
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

function polyMod (buffer) {
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

export {
  convertBit,
  polyMod,
  ALPHABET,
  ALPHABET_MAP
}
