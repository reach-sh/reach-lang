import * as cfxsdk from 'js-conflux-sdk';
import * as cfxers from './cfxers';
import {TypeDefs} from './classy_TypeDefs';
import {CFX_TypeDefs} from './classy_TypeDefs_ETH_like';
// import {BigNumber} from 'ethers';

export interface ReachStdlib_Opts {
  readonly REACH_DEBUG?: boolean
  readonly REACH_CONNECTOR_MODE?: string
}

export interface ETH_Like_Opts extends ReachStdlib_Opts {
  readonly ethers?: any
  readonly provider?: any
}

export interface CFX_Opts extends ETH_Like_Opts {
  readonly CFX_DEBUG?: boolean
  readonly CFX_NODE_URI?: string
  readonly CFX_NETWORK_ID?: string | number
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

  /** @deprecated */
  setDEBUG(b: boolean) {
    if (typeof b !== 'boolean') throw Error(`setDEBUG expects a boolean, got: '${b}'`);
    // XXX We are turning a blind eye to mutation for now,
    // but later should delete setDEBUG and only set it via the opts.
    // @ts-ignore
    this.opts.REACH_DEBUG = b;
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
  }

}

// const cfx = new CFX();
