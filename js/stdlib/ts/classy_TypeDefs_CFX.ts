import { ETH_T_Address, ETH_TypeDefs, unwrapAddress } from './classy_TypeDefs_ETH_like';
import { CFX_Opts } from './classy_opts';
import { A_T_Address } from './classy_TypeDefs';

function assertCfxAddress(s: string, prefix: string) {
  if (!s.startsWith(`${prefix}:`)) throw Error(`Expected address '${s}' to start with prefix '${prefix}'`);
  // +1 for the colon after the prefix
  const expectedLength = 42;
  const unprefixed = s.split(':').slice(-1)[0];
  if (unprefixed.length !== expectedLength) throw Error(`Expected address string '${s}' to be length ${expectedLength}, but it was ${s.length}.`);
  // TODO: more CFX address verification. Check the chechsum?
}

export class CFX_T_Address extends ETH_T_Address {
  readonly defaultValue: string
  prefix: string
  constructor(opts: CFX_Opts) {
    super();
    const {networkId} = opts;
    // TODO: support other networkIds
    // TODO: once we support mainnet & others, it's not just `net${networkId}` anymore
    if (networkId !== 999) throw Error(`XXX netId !== 999`);
    this.prefix = `NET${networkId}`;
    // TODO: default value based on netId.
    // (requires figuring out a checksum which includes netId)
    // A random address.
    this.defaultValue = `${this.prefix}:aajj5n713r16x9y44re0sxtnbw35km5gbua78nhdz7`
  }
  // TODO: more checking for correct CFX address format
  // Possibly: conversion from ETH-style address
  canonicalize(uv: string): string {
    // TODO: are there better choices re: upper case?
    const unwrapped = (unwrapAddress(uv) || uv).toUpperCase();
    assertCfxAddress(uv, this.prefix);
    if (typeof unwrapped !== 'string') throw Error(`Expected address, but got ${uv}`);
    return unwrapped;
  }
}

export class CFX_T_Token extends CFX_T_Address implements A_T_Address {
  // T_Token on CFX is CFX_T_Address in all but name
  name = 'Token'
}

export class CFX_TypeDefs extends ETH_TypeDefs {
  readonly T_Address: CFX_T_Address
  readonly T_Token: CFX_T_Token
  constructor(opts: CFX_Opts) {
    super();
    this.T_Address = new CFX_T_Address(opts);
    this.T_Token = new CFX_T_Token(opts);
  }
}
