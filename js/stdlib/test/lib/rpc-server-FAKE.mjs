import * as RPC    from '@reach-sh/stdlib/rpc_server.mjs';
import * as FAKE   from '@reach-sh/stdlib/FAKE.mjs';
import * as common from './common.mjs';


export const spec = async () => {
  const { rpc_stdlib } = await RPC.mkStdlibProxy(FAKE);

  await common.describe('The `FAKE` RPC server', async () => {
    await common.mkStdlibNetworkCommon(rpc_stdlib);
    // TODO await common.mkGetDefaultAccount(FAKE);
    // TODO await common.mkNewAccountFromSecret(rpc_stdlib,   14, sec);
    // TODO await common.mkNewAccountFromMnemonic(rpc_stdlib, 14, mon);
    await common.mkConnectAccount(rpc_stdlib, a => a);
  });
};
