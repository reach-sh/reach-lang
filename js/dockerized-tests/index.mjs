import * as common from './lib/common.mjs';


(async () => {
  const mode = process.env.REACH_CONNECTOR_MODE
    .replace(/-.*$/, '');

  const stdlib    = await import(`./lib/stdlib-${mode}.mjs`);
  const rpcServer = await import(`./lib/rpc-server-${mode}.mjs`);

  await common.run([ stdlib.spec, rpcServer.spec ]);
})();
