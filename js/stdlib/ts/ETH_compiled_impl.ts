import * as CBR from './CBR';
import type { CBR_Address } from './CBR';
import type { ETH_Ty } from './ETH_like_interfaces';

function addressUnwrapper(x: any): string {
  if (typeof x === 'string') {
    // XXX is this actually needed?
    if (x.slice(0, 2) !== '0x') {
      return '0x' + x;
    } else {
      return x;
    }
  } else if (x.networkAccount && x.networkAccount.address) {
    return (x.networkAccount.address);
  } else if (x.address) {
    return x.address;
  } else {
    throw Error(`Failed to unwrap address ${x}`);
  }
}

export const T_Address: ETH_Ty<CBR_Address, string> = {
  ...CBR.BT_Address,
  canonicalize: (uv: unknown): CBR_Address => {
    const val = addressUnwrapper(uv);
    return CBR.BT_Address.canonicalize(val || uv);
  },
  defaultValue: '0x' + Array(40).fill('0').join(''),
  munge: (bv: CBR_Address): string => bv,
  unmunge: (nv: string): CBR_Address => T_Address.canonicalize(nv),
  paramType: 'address',
}
