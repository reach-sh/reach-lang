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


  // Supply ZMD and GIL to account
  const gimmeTokens = async (acc) => {
    await zmd.mint(acc, startingBalance);
    await gil.mint(acc, startingBalance);
  }


  // Create & Fund Admin
  const accAdmin = await stdlib.newTestAccount(startingBalance);
  await gimmeTokens(accAdmin);


  // Create & Fund Providers
  const accProviders = await Promise.all(
    Array.from({ length: NUM_PROVIDERS }, () =>
      stdlib.newTestAccount(startingBalance))
  );

  for (let i = 0; i < accProviders.length; ++i) {
    const accProvider = accProviders[i];
    await gimmeTokens(accProvider);
  }


  // Deploy contract
  const ctcAdmin = accAdmin.deploy(backend);
  const ctcInfo = ctcAdmin.getInfo();


  // Track who withdrew/deposited
  const withdrew  = {};
  const deposited = {};


  // Admin backend
  const adminBackend = backend.Admin(ctcAdmin, {
    tokAAmt: stdlib.parseCurrency(20),
    tokBAmt: stdlib.parseCurrency(10),
    tokA: zmd.id,
    tokB: gil.id,
    shouldClosePool: ([ isAlive, market ]) => {
      const everyoneWent = false;
      console.log(`Admin will ${everyoneWent ? '' : 'not '}close pool`);
      return { when: everyoneWent, msg: null };
    },
  });


  // Provider backends
  const provBackends = accProviders.map((accProvider, i) => {
    const ctcProvider = accProvider.attach(backend, ctcInfo);
    const who = `Provider ${i}`;
    withdrew[who] = false;
    deposited[who] = false;
    return backend.Provider(ctcProvider, {
      ...stdlib.hasConsoleLogger,
      withdrawDone: (isMe, amtOuts) => {
        if (isMe) {
          withdrew[who] = true;
          console.log(`${who} withdrew ${amtOuts}`);
        }
      },
      withdrawMaybe: ([ alive, market ]) => {
        if (deposited[who]) {
          return { when: true, msg: { liquidity: deposited[who] } };
        } else {
          return { when: false, msg: { liquidity: 0 }};
        }
      },
      depositMaybe: ([ isAlive, market ]) => {
        const amt = Math.floor(Math.random() * 10);
        const deposit = {
          amtA: stdlib.parseCurrency(amt * 2), // * k
          amtB: stdlib.parseCurrency(amt),
        };
        console.log(`${who} tries to deposit: ${deposit.amtA} ZMD & ${deposit.amtB} GIL`);
        return { when: true, msg: deposit };
      },
      depositDone: (isMe, amtA, amtB, poolTokens) => {
        if (isMe) {
          deposited[who] = poolTokens;
          console.log(`${who} received ${poolTokens} pool tokens for their deposit of ${amtA} ZMD & ${amtB} GIL`);
        }
      }
    });
  });

  const backends = [ adminBackend, provBackends ];
  await Promise.all(backends);

  console.log(`Uniswap finished`);

})();
