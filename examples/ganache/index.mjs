import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import ethers from 'ethers';
import ganache from 'ganache-core';
const stdlib = loadStdlib();

(async () => {
  // OPTIONAL GANACHE CONFIG THAT CAN BE PASSED into ganache.provier()
  const options = {
    total_accounts: 1000,
    gasPrice: '21000',
    gasLimit: '5000000',
    hardfork: "petersburg",
    default_balance_ether: 7777777,
    port: 7545,
    ws: true,
    vmErrorsonRPCResponse: true,
    debug: true,
    allowUnlimitedContractSize: true
  };
  //pass ganacheProvider into setProvider to run a reach program in a ganache env
  const ganacheProvider = new ethers.providers.Web3Provider(ganache.provider(options));
  await stdlib.setProvider(ganacheProvider);
  const accAlice = await stdlib.connectAccount(ganacheProvider.getSigner(999));

  //Alice connects to ganacheAcc #999 with default balance of 7777777ETH
  const balance = await stdlib.balanceOf(accAlice);
  const addr = stdlib.formatAddress(accAlice);
  const bal = stdlib.formatCurrency(balance);
  console.log(ganacheProvider);
  console.log([`${addr} has ${bal} ${stdlib.standardUnit}`]);


  const ctcAlice = accAlice.deploy(backend);
  await Promise.all([
    backend.Alice(ctcAlice, {
      ...stdlib.hasRandom,
    }),
  ]);
  console.log('Alice has the correct balance of ganache default 7777777 ETH;')
})();