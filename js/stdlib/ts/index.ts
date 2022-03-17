export * as ask from './ask';
export {loadStdlib, unsafeAllowMultipleStdlibs} from './loader';
export {getConnector, getConnectorMode} from './ConnectorMode';
export * as rpc_server from './rpc_server';

import MyAlgoConnect from './ALGO_MyAlgoConnect';
import WalletConnect from './ALGO_WalletConnect';

export const wallets = {
  ALGO: {
    MyAlgoConnect,
    WalletConnect,
  },
};
