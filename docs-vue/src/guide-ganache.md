# {#guide-ganache} How to use ganache as a provider in your Reach program.
Ganache is a very popular Ethereum perosnal blockchain for quick application development,deployment and testing with a replica of ethereum mainnet.
Compared to using Reach dockerized devnet, ganache can provide you with a predefined number of test account,ETH balance, cusotm gas limit, price and many more options such as an exact hard fork of Ethereums live mainnet.
Import into your package.json `ganache-core`, currently the latest version is 2.13.2
Then in your inex.mjs import ganache-core and ethersjs
```js
import ganache from 'ganache-core';
import ethers from 'ethers';
```

Start by initializing ganacheProvider as a new ethers Web3Provider, this will run ganache node internally.
Then we setProvider for the Reach program as ganacheProvider using the Reach stdlib.
```js
(async () => {
  const ganacheProvider = new ethers.providers.Web3Provider(ganache.provider());
  await stdlib.setProvider(ganacheProvider);
...
```

Ganache by default gives us 10 test accounts each with 100 ETH, so to provider Alice newTestAccount with funds from the ganache preset accounts
we need to set one of the accounts as our faucet for Alice. Here `ganacheProvider.getSigner()` to get the default indexed account from ganache, you can also pass in which number account you want to obtain and set as faucet by indication which indexed account number you want to getSigner of.

Then we setFaucet for our reach program using stdlib.setFaucet to the account of (faucet).
```js

  const faucet = ganacheProvider.getSigner();
  await stdlib.setFaucet(stdlib.connectAccount(faucet));

  const startingBalance = stdlib.parseCurrency(10);
  const accAlice = await stdlib.newTestAccount(startingBalance)
```

How can I change options for my ganache test environment?
Below are some options you can make to customize your local ganache tests. I am setting the default balance for all 15 accounts to 10000ETH each and using the byzantium fork of Ethereum.
```js


(async () => {
  const options = {
      default_balance_ether: 10000,
      total_accounts: 15,
      hardFork: "byzantium",
      allowUnlimitedContractSize: true,
      gasLimit: 21000,
  }
  const ganacheProvider = new ethers.providers.Web3Provider(ganache.provider(options));
  await stdlib.setProvider(ganacheProvider);

  const faucet = ganacheProvider.getSigner();
  await stdlib.setFaucet(stdlib.connectAccount(faucet));

  const startingBalance = stdlib.parseCurrency(10);
  const accAlice = await stdlib.newTestAccount(startingBalance)

```
These are just some of the many options ganache give a developer and you can optimize them to fit your applications needs.