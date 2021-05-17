import * as eci from './ETH_compiled_impl';
import type { ETH_Ty } from './ETH_like_interfaces';

export const T_Address: ETH_Ty<string, string> = {
  ...eci.T_Address,
  canonicalize: (uv: unknown): string => uv as string, // XXX
  defaultValue: 'XXX', // XXX
  munge: (bv: string): string => bv,
  unmunge: (nv: string): string => T_Address.canonicalize(nv),
}
