import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/announcer.main.mjs';
import * as poolBackend from './build/index.main.mjs';
import * as ask from '@reach-sh/stdlib/ask.mjs';

export const runManager = async (useTestnet) => {
  const stdlib = await loadStdlib();
  let manager;
  if (useTestnet) {
    stdlib.setProviderByName('TestNet');
    const secret = await ask.ask(`What is your secret key (ETH) / mnemonic (ALGO) ?`);
    manager = await (stdlib.connector == 'ALGO'
      ? stdlib.newAccountFromMnemonic(secret)
      : stdlib.newAccountFromSecret(secret));
  } else {
    const startingBalance = stdlib.parseCurrency(100);
    manager = await stdlib.newTestAccount(startingBalance);
  }

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

export const runListener = async (useTestnet) => {
  const stdlib = await loadStdlib();
  let accListener;
  if (useTestnet) {
    stdlib.setProviderByName('TestNet');
    const secret = await ask.ask(`What is your secret key (ETH) / mnemonic (ALGO) ?`);
    accListener = await (stdlib.connector == 'ALGO'
      ? stdlib.newAccountFromMnemonic(secret)
      : stdlib.newAccountFromSecret(secret));
  } else {
    const startingBalance = stdlib.parseCurrency(100);
    accListener = await stdlib.newTestAccount(startingBalance);
  }

  if (stdlib.connector == 'ETH') {
    accListener.setGasLimit(5000000);
  }
  const listenerInfo = await ask.ask(`Paste Announcer Contract Info:`);

  await runListener_(stdlib, accListener, listenerInfo)();
}

export const runListener_ = (stdlib, accListener, listenerInfo) => async () => {
  const ctcListener = accListener.attach(backend, stdlib.connector == 'ALGO' ? parseInt(listenerInfo) : listenerInfo)

  const backendListener = backend.Listener(ctcListener, {
    hear: async (poolInfo) => {
      console.log(`\x1b[2m`, `Pool ID:`, poolInfo, '\x1b[0m');
      const ctcInfo = stdlib.connector == 'ALGO' ? parseInt(poolInfo) : poolInfo;
      const ctcPool = await accListener.attach(poolBackend, ctcInfo);
      const views = await ctcPool.getViews();
      const tokA = await views.Tokens.aTok();
      const tokB = await views.Tokens.bTok();
      let aBal = await views.Tokens.aBal();
      let bBal = await views.Tokens.bBal();
      if (tokA[0] == 'None' || tokB[0] == 'None') {
        console.log(`XXX: Pool ${poolInfo} does not have token info`);
        return;
      }

      const resA = await accListener.tokenMetadata(tokA[1]);
      const tokASym = resA.symbol;
      const tokABal = (aBal[0] == 'None') ? 0 : aBal[1];
      console.log(`\x1b[2m`, `  *`, tokABal.toString(), tokASym, '\x1b[0m');

      const resB = await accListener.tokenMetadata(tokB[1]);
      const tokBSym = resB.symbol;
      const tokBBal = (bBal[0] == 'None') ? 0 : bBal[1];
      console.log(`\x1b[2m`, `  *`, tokBBal.toString(), tokBSym, '\x1b[0m');

      const info = {
        poolAddr: poolInfo,
        tokA: {
          ...resA,
          id: tokA[1]
        },
        tokB: {
          ...resB,
          id: tokB[1]
        }
      };
      console.log(`\x1b[2m`, `  * Info: ${JSON.stringify(info)}`, '\x1b[0m')
    },
  });

  console.log(`Listening...`);
  return Promise.all([ backendListener ]);
}
