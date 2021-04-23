import cfxsdk from 'js-conflux-sdk';
import * as cfxers from './cfxers';
import { ReachStdlib } from './classy_shared';
import { CFX_Opts, ETH_Like_Opts } from './classy_opts';
import {CFX_TypeDefs} from './classy_TypeDefs_CFX';
import {ETH_TypeDef} from './classy_TypeDefs_ETH_like';
import ethers from 'ethers';

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
  prepForDigest(t: ETH_TypeDef<unknown, unknown>, v: unknown): string|[] {
    // Note: abiCoder.encode doesn't correctly handle an empty tuple type
    if (t.paramType === 'tuple()') {
      if (Array.isArray(v) && v.length === 0) {
        return [];
      } else {
        throw Error(`impossible: digest tuple() with non-empty array: ${JSON.stringify(v)}`);
      }
    }
    return ethers.utils.defaultAbiCoder.encode([t.paramType], [t.munge(v)])
  }
  tokenEq(a: unknown, b: unknown): boolean {
    const {T_Token} = this.typeDefs;
    return this.bytesEq(T_Token.canonicalize(a), T_Token.canonicalize(b));
  }
  async getProvider(): Promise<Provider> {
    // TODO: wait-port
    return this.provider;
  }
  async connectAccount(networkAccount: unknown): Promise<unknown> {
    void(networkAccount);
    return {};
  }
  async newAccountFromSecret(secret: string): Promise<unknown> {
    const provider = await this.getProvider();
    const networkAccount = (new this.ethers.Wallet(secret)).connect(provider);
    const acc = await this.connectAccount(networkAccount);
    return acc;
  }
}

export function parseNetworkId(opts: CFX_Opts) {
  return opts.networkId
    || (opts.CFX_NETWORK_ID && parseInt(opts.CFX_NETWORK_ID.toString()))
    || CFX.DEFAULT_CFX_NETWORK_ID;
}

export class CFX extends ETH_Like<cfxers.providers.Provider> {
  static readonly DEFAULT_CFX_NODE_URI = 'http://localhost:12537';
  static readonly DEFAULT_CFX_NETWORK_ID = 999;
  readonly opts: CFX_Opts;
  readonly standardUnit = 'CFX';
  readonly typeDefs: CFX_TypeDefs;
  constructor(opts: CFX_Opts = {}) {
    super({
      ethers: cfxers,
      provider: new cfxers.providers.Provider(
        new cfxsdk.Conflux({
          url: opts.CFX_NODE_URI || CFX.DEFAULT_CFX_NODE_URI,
          logger: opts.CFX_DEBUG ? console : undefined,
          networkId: parseNetworkId(opts),
        }),
      ),
      ...opts,
    });
    this.opts = {
      networkId: parseNetworkId(opts),
      ...super.opts,
    };
    this.typeDefs = new CFX_TypeDefs(this.opts);
  }
}

// const cfx = new CFX();
