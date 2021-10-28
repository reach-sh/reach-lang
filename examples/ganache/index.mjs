import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import ethers from 'ethers';
import ganache from 'ganache-core';

(async () => {
  const stdlib = loadStdlib(process.env);
  if ( stdlib.connector !== 'ETH' ) { return; }

  const ganacheOptions = {};
  const ganacheProvider =
    new ethers.providers.Web3Provider(
      ganache.provider(ganacheOptions));
  stdlib.setProvider(ganacheProvider);

  const faucet = ganacheProvider.getSigner();
  stdlib.setFaucet(stdlib.connectAccount(faucet));

  const startingBalance = stdlib.parseCurrency(50);
  const accAlice = await stdlib.newTestAccount(startingBalance)

  const ctcAlice = accAlice.deploy(backend);
  await Promise.all([
    backend.Alice(ctcAlice, {
      ...stdlib.hasRandom,
    }),
  ]);
})();
