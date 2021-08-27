import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import ethers from 'ethers';
import ganache from 'ganache-core';
const stdlib = loadStdlib();

(async () => {
  const ganacheProvider = new ethers.providers.Web3Provider(ganache.provider());
  await stdlib.setProvider(ganacheProvider);

  const faucet = ganacheProvider.getSigner();
  await stdlib.setFaucet(stdlib.connectAccount(faucet));

  const startingBalance = stdlib.parseCurrency(10);
  const accAlice = await stdlib.newTestAccount(startingBalance)

  const ctcAlice = accAlice.deploy(backend);
  await Promise.all([
    backend.Alice(ctcAlice, {
      ...stdlib.hasRandom,
    }),
  ]);
})();