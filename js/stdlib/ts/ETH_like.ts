import * as cfxsdk from 'js-conflux-sdk';
import * as cfxers from './cfxers';
import {BigNumber} from 'ethers';

// idk why VSCode says this is a parse error
export abstract class Nonsense { }

export abstract class TypeDef {
}

export abstract class TypeDefs {
  abstract readonly T_UInt: TypeDef
  abstract readonly T_Address: TypeDef
}

export abstract class ETH_TypeDef<T> extends TypeDef {
  abstract canonicalize(uv: unknown): T
}

class ETH_T_UInt extends ETH_TypeDef<BigNumber> {
  canonicalize(uv: unknown): BigNumber {
    // XXX
    return uv as BigNumber;
  }
}

class ETH_T_Address extends ETH_TypeDef<string> {
  canonicalize(uv: unknown): string {
    // XXX
    return uv as string;
  }
}

class CFX_T_Address extends ETH_TypeDef<string> {
  canonicalize(uv: unknown): string {
    // XXX
    return uv as string;
  }
}

export abstract class ETH_Like_TypeDefs extends TypeDefs {
  readonly T_UInt = new ETH_T_UInt()
}

export class ETH_TypeDefs extends ETH_Like_TypeDefs {
  readonly T_Address: ETH_TypeDef<string> = new ETH_T_Address();
}

export class CFX_TypeDefs extends ETH_Like_TypeDefs {
  readonly T_Address: ETH_TypeDef<string> = new CFX_T_Address();
}

export abstract class ReachStdlib {
  abstract readonly typeDefs: TypeDefs
  abstract readonly standardUnit: string

  readonly opts: ReachStdlib_Opts;

  constructor(opts: ReachStdlib_Opts = {}) {
    opts = {
      REACH_DEBUG: false,
      REACH_CONNECTOR_MODE: 'ETH',
      ...opts,
    }
    this.opts = opts;
  }
}

export abstract class ETH_Like<Provider> extends ReachStdlib {
  readonly ethers: any
  readonly provider: Provider
  readonly opts: ETH_Like_Opts

  constructor(opts: ETH_Like_Opts = {}) {
    super(opts);

    if (!opts.ethers) throw Error(`impossible: ethers is missing`);
    if (!opts.provider) throw Error(`impossible: provider is missing`);

    this.ethers = opts.ethers;
    this.provider = opts.provider;
    this.opts = super.opts;
  }

}

export interface ReachStdlib_Opts {
  REACH_DEBUG?: boolean
  REACH_CONNECTOR_MODE?: string
}

export interface ETH_Like_Opts extends ReachStdlib_Opts{
  ethers?: any
  provider?: any
}

export interface CFX_Opts extends ETH_Like_Opts {
  CFX_DEBUG?: boolean
  CFX_NODE_URI?: string
  CFX_NETWORK_ID?: string | number
}

export class CFX extends ETH_Like<cfxers.providers.Provider> {
  static readonly DEFAULT_CFX_NODE_URI = 'http://localhost:12537';
  static readonly DEFAULT_CFX_NETWORK_ID = 999;

  readonly opts: CFX_Opts;

  readonly standardUnit = 'CFX';
  readonly typeDefs: CFX_TypeDefs = new CFX_TypeDefs();

  constructor(opts: CFX_Opts = {}) {
    super({
      ethers: cfxers,
      provider: new cfxers.providers.Provider(
        new cfxsdk.Conflux({
          url: opts.CFX_NODE_URI || CFX.DEFAULT_CFX_NODE_URI,
          logger: opts.CFX_DEBUG ? console : undefined,
          networkId: (opts.CFX_NETWORK_ID && parseInt(opts.CFX_NETWORK_ID.toString()))
            || CFX.DEFAULT_CFX_NETWORK_ID,
        }),
      ),
      ...opts,
    });
    this.opts = super.opts;
    // this.conflux = this.provider.conflux;
  }

}

const cfx = new CFX();
