export interface ReachStdlib_Opts {
  // XXX REACH_DEBUG?: string
  readonly REACH_DEBUG?: boolean
  readonly REACH_CONNECTOR_MODE?: string
  readonly REACH_FAUCET_SECRET?: string
  readonly REACH_FAUCET_MNEMONIC?: string
}

export interface ETH_Like_Opts extends ReachStdlib_Opts {
  readonly ethers?: any
  readonly provider?: any
}

export interface ETH_Opts extends ETH_Like_Opts {
  readonly ETH_NODE_URI?: string
}

export interface CFX_Opts extends ETH_Like_Opts {
  // XXX CFX_DEBUG?: string
  readonly CFX_DEBUG?: boolean
  readonly CFX_NODE_URI?: string
  readonly CFX_NETWORK_ID?: string | number
  readonly networkId?: number
}
