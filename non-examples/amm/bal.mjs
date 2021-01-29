import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const NUM_PROVIDERS = 2;
const NUM_TRADERS   = 2;
const NUM_TOKENS    = 3;

const tokenRatio = [50, 25, 25];

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
    getParams: () => ({
      totalWeight: 100,
      weights: tokenRatio,
      swapFee: 0.03,
    }),
    shouldClosePool: everyOneTraded,
  });

  const providersB = accProviders.map((accProvider, i) => {
    const ctcProvider = accProvider.attach(backend, ctcInfo);
    const who = `Provider ${i}`;
    // Have even `i`s do allAssetWithdrawal, odd do singleAssetWithdrawal
    const iAmEven = i % 2 == 0;
    let iDeposited = false;
    return backend.Provider(ctcProvider, {
      allAssetWithdrawalMaybe: ([ alive, pool, market ]) =>
        (iAmEven && iDeposited && everyOneTraded())
          ? [ true, { liquidity: providerPoolTokens[i] } ]
          : [ false, { liquidity: 0 }],
      allAssetWithdrawalDone: (amtOuts) =>
        console.log(`${who} withdrew ${amtOuts} via allAssetWithdrawal`),
      allAssetDepositMaybe: ([ alive, pool, market ]) => {
        if (iAmEven && !iDeposited) {
          iDeposited = true;
          const amtIns = [4, 2, 2];
          console.log(`${who} will deposit ${amtIns} via allAssetDeposit`);
          return [ true, {
            amtIns,
            ratios: tokenRatio,
          }];
        } else {
          return [ false, { amtIns: mtArr }];
        }
      },
      allAssetDepositDone: (numOfPoolTokens) => {
        providerPoolTokens[i] = numOfPoolTokens;
        console.log(`${who} received ${numOfPoolTokens} for depositing via allAssetDeposit`);
      },
      singleAssetWithdrawalMaybe: ([ alive, pool, market ]) =>
        (!iAmEven && iDeposited && everyOneTraded())
          ? [ true, { liquidity: providerPoolTokens[i], outToken: 0 } ]
          : [ false, { liquidity: 0 }],
      singleAssetWithdrawalDone: (amtOut) =>
        console.log(`${who} withdrew ${amtOut} of Token 0 via singleAssetWithdrawal`),
      singleAssetDepositMaybe: ([ alive, pool, market ]) => {
        if (!iAmEven && !iDeposited) {
          iDeposited = true;
          console.log(`${who} will deposit ${1} of Token ${0} via singleAssetDeposit`);
          return [ true, { inToken: 0, amtIn: 1 }];
        } else {
          return [ false, { inToken: 0, amtIn: 0 }];
        }
      },
      singleAssetDepositDone: (numOfPoolTokens) => {
        providerPoolTokens[i] = numOfPoolTokens;
        console.log(`${who} received ${numOfPoolTokens} for depositing via singleAssetDeposit`);
      }
    });
  });

  const tradersB = accTraders.map((accTrader, i) => {
    const ctcTrader = accTrader.attach(backend, ctcInfo);
    const who = `Trader ${i}`;
    // Have even `i`s do swapExactAmountIn, odd do swapExactAmountOut
    const iAmEven = i % 2 == 0;
    return backend.Trader(ctcTrader, {
      swapExactAmountInMaybe : () => {
        if (iAmEven && !traderSwapActivity[i] ) {
          traderSwapActivity[i] = true;
          const amtIn = 1;
          const inToken  = 1;
          const outToken = 0;
          const minAmtOut = 1;
          return [ true, { amtIn, inToken, outToken, minAmtOut }];
        } else {
          return [ false, { amtIn: 0, inToken: 0, outToken: 0, minAmtOut: 0 } ];
        }
      },
      swapExactAmountInDone  : (amtOut) => {
        console.log(`${who} received ${amtOut} from swapExactAmountIn`);
      },
      swapExactAmountOutMaybe : () => {
        if (!iAmEven && !traderSwapActivity[i] ) {
          traderSwapActivity[i] = true;
          const amtOut = 1;
          const outToken = 0;
          const inToken  = 1;
          const maxAmtIn = 1;
          return [ true, { amtOut, inToken, outToken, maxAmtIn }];
        } else {
          return [ false, { amtOut: 0, inToken: 0, outToken: 0, maxAmtIn: 1 } ];
        }
      },
      swapExactAmountOutDone  : (amtOut) => {
        console.log(`${who} received ${amtOut} from swapExactAmountOut`);
      }
    });
  });

  const backends = [adminB, ...providersB, ...tradersB];

  await Promise.all(backends);

})();
