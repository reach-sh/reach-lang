import {ETH_T_Address, unwrapAddress} from './classy_TypeDefs_ETH_like';

export class CFX_T_Address extends ETH_T_Address {
  // TODO: more checking for correct CFX address format
  // Possibly: conversion from ETH-style address
  canonicalize(uv: string): string {
    const unwrapped = unwrapAddress(uv) || uv;
    if (typeof unwrapped !== 'string') throw Error(`Expected address, but got ${uv}`);
    return unwrapped;
  }
}
