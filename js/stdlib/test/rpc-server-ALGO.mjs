import { runTests, describe } from '../tester.mjs';

import * as RPC    from '../rpc_server.mjs';
import * as ALGO   from '../ALGO.mjs';
import * as common from './common.mjs';


runTests(async () => {
  const { rpc_stdlib } = await RPC.mkStdlibProxy(ALGO);

  await describe('The `ALGO` RPC server', async () => {
    await common.mkStdlibNetworkCommon(rpc_stdlib);
    // TODO await common.mkGetDefaultAccount(rpc_stdlib);
    // TODO await common.mkNewAccountFromSecret(rpc_stdlib,    8, sec);
    // TODO await common.mkNewAccountFromMnemonic(rpc_stdlib, 14, mon);
    await common.mkConnectAccount(rpc_stdlib, a => a);
    // TODO await common.mkFundFromFaucet(rpc_stdlib);
  });
});
