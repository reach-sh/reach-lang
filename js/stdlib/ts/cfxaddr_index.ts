// lightly adapted from @conflux-dev/conflux-address-js@1.0.0
// @ts-nocheck
import {
  ALPHABET,
  ALPHABET_MAP,
  polyMod,
  convertBit
} from './cfxaddr_base32'

const VERSION_BYTE = 0
const NET_ID_LIMIT = 0xFFFFFFFF

function encodeNetId (netId) {
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

function isValidNetId (netId) {
  return /^([1-9]\d*)$/.test(netId) && Number(netId) <= NET_ID_LIMIT
}

function decodeNetId (payload) {
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

function getAddressType (hexAddress) {
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

function encode (hexAddress, netId, verbose = false) {
  if (!(hexAddress instanceof Buffer)) {
    if (hexAddress instanceof Uint8Array) {
      hexAddress = Buffer.from(hexAddress)
    } else {
      throw new Error('hexAddress should be passed as a Buffer or Uint8Array')
    }
  }

  if (hexAddress.length < 20) {
    throw new Error('hexAddress should be at least 20 bytes')
  }

  const addressType = getAddressType(hexAddress).toUpperCase()
  const netName = encodeNetId(netId).toUpperCase()

  const netName5Bits = Buffer.from(netName).map(byte => byte & 0b11111)
  const payload5Bits = convertBit([VERSION_BYTE, ...hexAddress], 8, 5, true)

  const checksumBigInt = polyMod([...netName5Bits, 0, ...payload5Bits, 0, 0, 0, 0, 0, 0, 0, 0])
  const checksumBytes = Buffer.from(checksumBigInt.toString(16).padStart(10, '0'), 'hex')
  const checksum5Bits = convertBit(checksumBytes, 8, 5, true)

  const payload = payload5Bits.map(byte => ALPHABET[byte]).join('')
  const checksum = checksum5Bits.map(byte => ALPHABET[byte]).join('')

  return verbose
    ? `${netName}:TYPE.${addressType}:${payload}${checksum}`
    : `${netName}:${payload}${checksum}`.toLowerCase()
}

function decode (address) {
  // don't allow mixed case
  const lowered = address.toLowerCase()
  const uppered = address.toUpperCase()
  if (address !== lowered && address !== uppered) {
    throw new Error('Mixed-case address ' + address)
  }

  const [, netName, shouldHaveType, payload, checksum] = address.toUpperCase().match(/^([^:]+):(.+:)?(.{34})(.{8})$/)

  const prefix5Bits = Buffer.from(netName).map(byte => byte & 0b11111)
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

  if (shouldHaveType && `type.${type}:` !== shouldHaveType.toLowerCase()) {
    throw new Error('Type of address doesn\'t match')
  }

  const bigInt = polyMod([...prefix5Bits, 0, ...payload5Bits, ...checksum5Bits])
  if (Number(bigInt)) {
    throw new Error(`Invalid checksum for ${address}`)
  }

  return { hexAddress, netId, type }
}

export { encode, decode }
