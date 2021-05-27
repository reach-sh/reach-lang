// TODO: migrate this file to .ts
import * as stdlib_loader from './loader.mjs';
import ETHcompiled from './token.sol.mjs';
import algosdk from 'algosdk';
import { ethers as real_ethers } from 'ethers';
import * as cfxers from './cfxers.mjs';
// import type { EthersLike } from './ETH_like_interfaces';

export default async function (name, sym) {
  // XXX update this to work in browser (process.env not directly available to libs)
  const stdlib = await stdlib_loader.loadStdlib();

  const startingBalance = stdlib.parseCurrency(10);
  console.log(`Launching token, ${name} (${sym})`);
  const accCreator = await stdlib.newTestAccount(startingBalance);

  const ETH_like_launchToken = async (ethers /*: EthersLike */) => {
    const remoteCtc = ETHcompiled["contracts"]["contracts/token/ERC20/presets/ERC20PresetMinterPauser.sol:ERC20PresetMinterPauser"];
    const remoteABI = remoteCtc["abi"];
    const remoteBytecode = remoteCtc["bin"];
    const factory = new ethers.ContractFactory(remoteABI, remoteBytecode, accCreator.networkAccount);
    console.log(`${sym}: deploy`);
    const contract = await factory.deploy(name, sym);
    console.log(`${sym}: wait for deploy: ${contract.deployTransaction.hash}`);
    const deploy_r = await contract.deployTransaction.wait();
    console.log(`${sym}: saw deploy: ${deploy_r.blockNumber}`);
    const id = contract.address;
    console.log(`${sym}: deployed: ${id}`);
    const mint = async (accTo, amt) => {
      const to = accTo.networkAccount.address;
      console.log(`${sym}: minting ${amt} ${sym} for ${to}`);
      const fn = await contract["mint"](to, amt);
      console.log(`${sym}: mint: wait`);
      await fn.wait();
    };
    const balanceOf = async (acc) => {
      const addr = acc.networkAccount.address;
      return await contract["balanceOf"](addr);
    };
    return { name, sym, id, mint, balanceOf };
  };

  const ETH_launchToken = async () => {
    return await ETH_like_launchToken(real_ethers);
  };

  const CFX_launchToken = async () => {
    return await ETH_like_launchToken(cfxers);
  };

  const ALGO_launchToken = async () => {
    const addr = (acc) => acc.networkAccount.addr;
    const caddr = addr(accCreator);
    const zaddr = caddr;
    // ^ XXX should be nothing; docs say can be "", but doesn't actually work
    const algod = await stdlib.getAlgodClient();
    const dotxn = async (mktxn, acc = accCreator) => {
      const sk = acc.networkAccount.sk;
      const params = await stdlib.getTxnParams();
      const t = mktxn(params);
      const s = t.signTxn(sk);
      const r = (await algod.sendRawTransaction(s).do());
      await stdlib.waitForConfirmation(r.txId);
      return await algod.pendingTransactionInformation(r.txId).do();
    };
    const ctxn_p = await dotxn(
      (params) =>
      algosdk.makeAssetCreateTxnWithSuggestedParams(
        caddr, undefined, Math.pow(2,48), 6,
        false, zaddr, zaddr, zaddr, zaddr,
        sym, name, '', '', params,
      ));
      const id = ctxn_p["asset-index"];
      console.log(`${sym}: asset is ${id}`);

      const mint = async (accTo, amt) => {
        console.log(`${sym}: minting ${amt} ${sym} for ${addr(accTo)}`);
        await stdlib.transfer(accCreator, accTo, amt, id);
      };
      const optOut = async (accFrom, accTo = accCreator) => {
        await dotxn(
          (params) =>
          algosdk.makeAssetTransferTxnWithSuggestedParams(
            addr(accFrom), addr(accTo), addr(accTo), undefined,
            0, undefined, id, params
          ), accFrom);
      };
      const balanceOf = async (accFrom) => {
        const taddr = addr(accFrom);
        console.log(`${sym}: balanceOf of ${taddr}`);
        const {assets} = await algod.accountInformation(taddr).do();
        for ( const ai of assets ) {
          if ( ai['asset-id'] === id ) {
            return ai['amount'];
          }
        }
        return false;
      };
      return { name, sym, id, mint, balanceOf, optOut };
  };
  const launchTokens = {
    'ETH': ETH_launchToken,
    'ALGO': ALGO_launchToken,
    'CFX': CFX_launchToken,
  };

  return await launchTokens[stdlib.connector]();
}
