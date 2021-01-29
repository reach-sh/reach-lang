import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const NUM_PROVIDERS = 2;
const NUM_TRADERS   = 2;

const tokenRatio = [1, 2];

const mtArr = tokenRatio.map(_ => 0);

const createPartClassAccounts = async (length) =>
  Promise.all(
    Array.from({ length }),
    () => stdlib.newTestAccount(startingBalance)
  )

(async () => {

  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(10);

  const accAdmin = await stdlib.newTestAccount(startingBalance);
  const accProviders = await createPartClassAccounts(NUM_PROVIDERS);
  const accTraders = await createPartClassAccounts(NUM_TRADERS);

  const ctcAdmin = accAdmin.deploy(backend);

  // Tracks how much pool tokens each provider has
  const providerPoolTokens =
    Array.from({ length: NUM_PROVIDERS }, _ => 0);

  // Tracks whether a trader has performed a swap
  const traderSwapActivity =
    Array.from({ length: NUM_TRADERS }, _ => false);

  const everyOneTraded = () =>
    traderSwapActivity.every(x => x);

  const adminB = backend.Admin(ctcAdmin, {
    formulaValuation: 100,
    shouldClosePool: everyOneTraded,
  });

  const providersB = accProviders.map((accProvider, i) => {
    const ctcProvider = accProvider.attach(backend, ctcInfo);
    const who = `Provider ${i}`;
    let iDeposited = false;
    return backend.Provider(ctcProvider, {
      withdrawMaybe: ([ alive, pool, market ]) =>
        (iDeposited && everyOneTraded())
          ? [ true, { liquidity: providerPoolTokens[i] } ]
          : [ false, { liquidity: 0 }],
      withdrawDone: (amtOuts) =>
        console.log(`${who} withdrew ${amtOuts}`),
      depositMaybe: ([ alive, pool, market ]) => {
        if (iDeposited) {
          return [ false, { amtIns: mtArr, ratios: tokenRatio }];
        } else {
          iDeposited = true;
          const amtIns = [2, 4];
          console.log(`${who} will deposit ${amtIns}`);
          return [ true, {
            amtIns,
            ratios: tokenRatio,
          }];
        }
      },
      depositDone: (numOfPoolTokens) => {
        providerPoolTokens[i] = numOfPoolTokens;
        console.log(`${who} received ${numOfPoolTokens} for depositing.`);
      }
    });
  });

  const tradersB = accTraders.map((accTrader, i) => {
    const ctcTrader = accTrader.attach(backend, ctcInfo);
    const who = `Trader ${i}`;
    return backend.Trader(ctcTrader, {
      tradeMaybe : () => {
        if ( traderSwapActivity[i] ) {
          return [ false, { amtIn: 0, inToken: 0, outToken: 0 } ];
        } else {
          traderSwapActivity[i] = true;
          const amtIn = 1;
          const inToken  = (i % 2 == 0) ? 0 : 1;
          const outToken = (i % 2 == 0) ? 1 : 0;
          return [ true, { amtIn, inToken, outToken }];
        }
      },
      tradeDone  : (amtOuts) => {
        console.log(`${who} received ${amtOuts} from swap`);
      }
    });
  });

  const backends = [adminB, ...providersB, ...tradersB];

  await Promise.all(backends);

})();
