import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/announcer.main.mjs';
import * as ask from '@reach-sh/stdlib/ask.mjs';

export const runManager = async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(99999999999);

  const manager = await stdlib.newTestAccount(startingBalance);
  if (stdlib.connector == 'ETH') {
    manager.setGasLimit(5000000);
  }

  const ctcManager = manager.deploy(backend);

  const cache = {};

  const backendManager = backend.Manager(ctcManager, {
    hear: (poolAddr) => {
      console.log(`Manager created pool:`, poolAddr);
    },
    printInfo: async () => {
      const info = await ctcManager.getInfo();
      console.log(`Announcer Contract Info: ${JSON.stringify(info)}`)
    },
    getPoolInfo: async () => {
      const poolAddr = await ask.ask(`Enter new pool address:`);
      cache[poolAddr] = true;
      return poolAddr;
    },
  });

  console.log(`Listening...`);
  await Promise.all([ backendManager ]);

};

export const runListener = async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(99999999999);

  const accListener = await stdlib.newTestAccount(startingBalance);
  if (stdlib.connector == 'ETH') {
    accListener.setGasLimit(5000000);
  }
  const listenerInfo = await ask.ask(`Paste Announcer Contract Info:`);
  const ctcListener = accListener.attach(backend, listenerInfo)

  const backendListener = backend.Listener(ctcListener, {
    hear: (poolInfo) => {
      console.log(`Manager added pool:`, poolInfo);
    },
  });

  console.log(`Listening...`);
  return Promise.all([ backendListener ]);
}
