import { loadStdlib } from '@reach-sh/stdlib';
import launchToken from '@reach-sh/stdlib/launchToken.mjs';
import * as backend from './build/index.main.mjs';

const NUM_PROVIDERS = 2;

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);

  // Create tokens to swap
  const zmd = await launchToken("zorkmid", "ZMD");
  const gil = await launchToken("gil", "GIL");

  const accAdmin = await stdlib.newTestAccount(startingBalance);
  const accProviders = await Promise.all(
    Array.from({ length: NUM_PROVIDERS }, () =>
      stdlib.newTestAccount(startingBalance))
  );

  const ctcAdmin = accAdmin.deploy(backend);
  const ctcInfo = ctcAdmin.getInfo();

  // How do I query the token for balanceOf[who] (Address)
  const providerValues = {};
  // Just used to see if everybody withdrew
  let counter = 0;

  const adminBackend = backend.Admin(ctcAdmin, {
    formulaValuation: 3,
    tokA: zmd.id,
    tokB: gil.id,
    shouldClosePool: ([ isAlive, market ]) => {
      console.log(`Admin will ${counter > 2 ? '' : 'not '}close pool`);
      return counter > 2;
    },
  });

  const provBackends = accProviders.map((accProvider, i) => {
    const ctcProvider = accProvider.attach(backend, ctcInfo);
    const who = `Provider ${i}`;
    return backend.Provider(ctcProvider, {
      withdrawDone: (isMe, amtOuts) => {
        if (isMe) {
          counter += 1;
          console.log(`${who} withdrew ${amtOuts}`);
        }
      },
      withdrawMaybe: (isAlive, market, pool) => {
        const providerPoolToks = providerValues[who];
        if (providerPoolToks != undefined) {
          console.log(`${who} wants to withdraw ${providerPoolToks}`);
          return { when: true, msg: providerPoolToks };
        } else {
          return { when: false, msg: { liquidity: 0 }};
        }
      }
    });
  });

  const backends = [ adminBackend, provBackends ];
  await Promise.all(backends);

  console.log(`Uniswap finished`);

})();
