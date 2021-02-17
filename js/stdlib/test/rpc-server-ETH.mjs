import { runTests, describe } from '../tester.mjs';

import * as RPC    from '../rpc_server.mjs';
import * as ETH    from '../ETH.mjs';
import * as common from './common.mjs';


runTests(async () => {
  const { rpc_stdlib } = await RPC.mkStdlibProxy(ETH);

  const sec = '9573fa33a57fee662a23bf60f1b1674364d99fb8dd2166b4ae470ce7ab20ed9f';

  await describe('The `ETH` RPC server', async () => {
    await common.mkStdlibNetworkCommon(rpc_stdlib);
    await common.mkGetDefaultAccount(rpc_stdlib);
    await common.mkNewAccountFromSecret(rpc_stdlib, 14, sec);
    // TODO await common.mkNewAccountFromMnemonic(rpc_stdlib, 14, mon);
    await common.mkConnectAccount(rpc_stdlib, a => a);
  });
});
