import { loadStdlib, util } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const { thread, Signal, Timeout } = util;

const useTestnet = process.argv.includes('testnet');
const stdlib = await loadStdlib();

const ready = new Signal();

if (useTestnet) {
  const rinkeby = {
    REACH_CONNECTOR_MODE: 'ETH-live',
    REACH_ISOLATED_NETWORK: 'no',
    ETH_NODE_NETWORK: 'rinkeby',
    ETH_NODE_URI: 'https://rinkeby.infura.io/v3/2c7550e5445f44bcbfcd747be6c50a77',
  }
  stdlib.setProviderByEnv(rinkeby);
}

const getAccount = async () => {
  if (useTestnet) {
    const secret = '0xc8be1f243bb1a22122e7b29ddb1402edeb29f8dfdc842cb53a6489d39242724e'; // await ask.ask('Testnet account secret: ');
    return await stdlib.newAccountFromSecret(secret);
  } else {
    return await stdlib.newTestAccount(stdlib.parseCurrency(100));
  }
}

const getSome = (mx) => {
  if (mx[0] == 'None') {
    throw Error('Expected Some, received None');
  } else {
    return mx[1];
  }
}

const acc = await getAccount();
acc.setGasLimit(5000000);

const ctc = acc.contract(backend);

const totalSupply = 2;

await Promise.all([
  backend.Deployer(ctc, {
    enum: {
      totalSupply,
    },
    meta: {
      name: 'wazowski',
      symbol: 'waz',
      tokenURI: 'https://i1.sndcdn.com/avatars-P9AimttHN0dhFha9-R0gNbA-t500x500.jpg',
    },
    zeroAddr: '0x0000000000000000000000000000000000000000',
    deployed: (ctcAddr) => {
      console.log(`Contract deployed @: `, stdlib.formatAddress(ctcAddr));
      ready.notify();
    },
    log: console.log
  }),
  thread(async () => {
    await ready.wait();
    console.log(`Contract is ready for interaction`);

    const accA = acc;
    const ctcA = await accA.contract(backend, ctc.getInfo());
    const I = ctcA.a;
    const V = ctcA.v;

    // Who is receiving the NFTs
    const accAddr = acc.getAddress();
    const to = accAddr;

    const mint = async (to, tokenId) => {
      console.log(`Minting ${tokenId} to ${to}`);
      await I.mint(to, tokenId);
    }

    // Mint the NFTs to the deployer
    for (let i = 0; i < totalSupply; i++) {
      await mint(to, i);
    }

    const ownerOf = async (tokenId) => {
      const owner = getSome(await V.ownerOf(tokenId));
      stdlib.assert(owner == to, `owner == to --- ${owner} == ${to}`)
      console.log(`${to} is the owner of token ${tokenId}`);
    }

    // Check that they own each one
    for (let i = 0; i < totalSupply; i++) {
      await ownerOf(i);
    }

    const transferFrom = async (from, to, tokenId) => {
      console.log(`transferFrom ${from} to ${to} : Token ${tokenId}`);
      await I.safeTransferFrom(from, to, tokenId);
    }

    for (let i = 0; i < totalSupply; i++) {
      await transferFrom(to, to, i);
    }

    process.exit();

  }),
]);

process.exit();
console.log('Done');
