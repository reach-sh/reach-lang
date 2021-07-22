import { loadStdlib } from '@reach-sh/stdlib';
import * as listenerBackend from './build/announcer.main.mjs';

export const runManager = async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(99999999999);

  const manager = await stdlib.newTestAccount(startingBalance);
  manager.setGasLimit(5000000);

  const ctcManager = manager.deploy(listenerBackend);

  const cache = {};

  const backendManager = listenerBackend.Manager(ctcManager, {
    hear: (poolInfo) => {
      console.log(`Manager hears!`, poolInfo);
      if (poolInfo in cache) {
        return;
      } else {
        cache[poolInfo] = true;
        console.log(poolInfo, `:`, true);
      }
    },
    printInfo: async () => {
      const info = await ctcManager.getInfo();
      console.log(`ctc info: ${JSON.stringify(info)}`)
    },
  });

  console.log(`Listening...`);
  await Promise.all([ backendManager ]);

};
