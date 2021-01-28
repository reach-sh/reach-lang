import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const chance = (n) => Math.random() < n;

const NUM_PROVIDERS = 2;
const NUM_TRADERS   = 2;

// Either make this more like
const tokenRatio = [1, 1.5];

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

  const fmt = x => stdlib.formatCurrency(x, 4);
  const getBalance = async (who) => fmt(await stdlib.balanceOf(who));

  const ctcAdmin = accAdmin.deploy(backend);

  const adminB = backend.Admin(ctcAdmin, {
    formulaValuation: 100,
  });

  const providersB = accProviders.map((accProvider, i) => {
    const ctcProvider = accProvider.attach(backend, ctcInfo);
    const who = `Provider ${i}`;
    return backend.Provider(ctcProvider, {});
  });

  const tradersB = accTraders.map((accTrader, i) => {
    const ctcTrader = accTrader.attach(backend, ctcInfo);
    const who = `Trader ${i}`;
    return backend.Trader(ctcTrader, {});
  });

  const backends = [adminB, ...providersB, ...tradersB];

  await Promise.all(backends);

  console.log(`Pool has been created.`);

  // Have providers deposit tokens
  const providerLiquidities = providersB.map((provider, i) => {
    const liquidity = provider.deposit({
      amtIns: [4, 6],
      ratios: tokenRatio,
    });
    console.log(`Provider ${i} deposited ${[4, 6]} at a ${ratios} ratio for ${liquidity} pool tokens.`);
    return liquidity;
  });

  // Have traders swap
  const makeTrade = (i, amtIn, amtInTok, amtOutTok) => {
    const amtOut = tradersB[i].trade({
      amtIn: 3,
      amtInTok: 1,
      amtOutTok: 0,
    });
    console.log(`Trader ${i} swapped ${amtIn} ${amtInTok} for ${amtOut} ${amtOutTok}`);
  };

  makeTrade(0, 2, 0, 1);
  makeTrade(1, 3, 1, 0);

  // Have one provider withdraw their liquidity
  const tokenAmts = providersB[0].withdraw(providerLiquidities[0]);
  console.log(`Provider 0 withdrew ${providerLiquidities[0]} for: ${tokenAmts}`);

  // Have admin close pool
  adminB.closePool();
  console.log(`Admin has closed pool.`);

  // Have other provider withdraw their liquidity
  const tokenAmts1 = providersB[1].withdraw(providerLiquidities[1]);
  console.log(`Provider 1 withdrew ${providerLiquidities[1]} for: ${tokenAmts1}`);

  console.log(`Pool has been closed.`);

})();
