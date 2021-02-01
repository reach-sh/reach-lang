// ****************************************************************************
// standard library needed at runtime by compiled Reach programs
// ****************************************************************************

import * as shared from './shared';
import { typeDefs as EthTypes, addressEq, digest, UInt_max } from './ETH_compiled';


export const typeDefs = EthTypes;

export const stdlib = {
  ...shared,
  ...EthTypes,
  addressEq,
  digest,
  UInt_max,
};
