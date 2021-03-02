import * as RPC    from '@reach-sh/stdlib/rpc_server.mjs';
import * as ALGO   from '@reach-sh/stdlib/ALGO.mjs';
import * as common from './common.mjs';


export const spec = async () => {
  const { rpc_stdlib } = await RPC.mkStdlibProxy(ALGO);

  await common.describe('The `ALGO` RPC server', async () => {
    await common.mkStdlibNetworkCommon(rpc_stdlib);
    // TODO await common.mkGetDefaultAccount(rpc_stdlib);
    // TODO await common.mkNewAccountFromSecret(rpc_stdlib,    8, sec);
    // TODO await common.mkNewAccountFromMnemonic(rpc_stdlib, 14, mon);
    await common.mkConnectAccount(rpc_stdlib, a => a);

    await common.mkKont(rpc_stdlib);
  });
};
