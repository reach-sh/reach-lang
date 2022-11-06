import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import * as fs from 'fs';

const failingMethod = 2;

const stdlib = loadStdlib(process.env);
const pc = stdlib.parseCurrency;
const b = pc(100);
const balOf = async (acc, tok) => stdlib.balanceOf(acc, tok);
const amt = pc(20);

const accAlice = await stdlib.newTestAccount(b);
const ctcA = accAlice.contract(backend);

const algo = async () => {
  const token = await stdlib.launchToken(accAlice, "Zorkmid", "ZMD", { clawback: accAlice });
  console.log(`Launched token:`, token.id.toString());

  const { algosdk } = stdlib;
  const tokenId = token.id.toNumber();

  const triggerClawback = async (addr, funds) => {
    console.log(`Clawback has been TRIGGERED!`);
    const address = stdlib.formatAddress(addr);
    const aliceAddr = accAlice.networkAccount.addr;
    try {
      const params = await stdlib.getTxnParams('');
      const rtxns = [await algosdk.makeAssetTransferTxnWithSuggestedParams(
        aliceAddr, aliceAddr,
        undefined, address,
        20000000,
        undefined,
        tokenId, params
      )];
      await algosdk.assignGroupID(rtxns);
      const wtxns = rtxns.map(stdlib.toWTxn);
      const res = await stdlib.signSendAndConfirm(accAlice.networkAccount, wtxns);
      console.log(res);
    } catch (e) {
      console.log(`err:`, e);
    }
  }

  const checkBal = async (addr, idx) => {
    const address = stdlib.formatAddress(addr);
    const bal = await balOf(address, tokenId);
    const expectedBal = {
        0: amt,
        1: pc(0),
      }[idx];
    stdlib.assert(bal.eq(expectedBal), `Expected correct balance: ${bal} == ${expectedBal}`);
    console.log(`Balance:`, bal.toString());
  }

  try {
    await Promise.all([
      backend.Alice(ctcA, {
        amt,
        token: tokenId,
        triggerClawback,
        checkBal,
        ...stdlib.hasConsoleLogger,
      })
    ]);
  } catch (e) {
    const expFailMethod = `_reachp_${failingMethod}`;
    const es = e.toString();
    stdlib.assert(es.includes(expFailMethod) || es.includes(`underflow`));
    console.log(`Error was thrown in the expected method: ${expFailMethod}`);
    return;
  }
  stdlib.assert(false, "Expected an error to be thrown when transferring funds that were clawbacked");
}

const eth = async () => {
  const { ethers } = stdlib;

  const myGasLimit = 5000000;
  accAlice.setGasLimit(myGasLimit);
  const supply = pc(30);

  const compiled = JSON.parse(await fs.readFileSync('./build/index.sol.json'));
  const remoteCtc = compiled["contracts"]["index.sol:ERC20"];
  const remoteABI = remoteCtc["abi"];
  const remoteBytecode = remoteCtc["bin"];
  const factory = new ethers.ContractFactory(remoteABI, remoteBytecode, accAlice.networkAccount);
  const contract = await factory.deploy("Zorkmid", "ZMD", { gasLimit: myGasLimit });
  await contract.deployTransaction.wait();
  const tokenId = contract.address;
  const t = await contract["mint"](supply);
  await t.wait();

  const triggerClawback = async (addr, funds) => {
    console.log(`Clawback was triggered`, funds.toString());
    await stdlib.doCall('clawback', contract, "closeOut", [addr], 0);
  }

  const checkBal = async (addr, idx) => {
    const bal = await stdlib.balanceOf(addr, tokenId);
    console.log(`Balance:`, JSON.stringify(bal));
    const expectedBal = {
      0: amt,
      1: pc(0),
    }[idx];
    stdlib.assert(bal.eq(expectedBal), `checkBal: expected ${expectedBal}, got ${bal}`);
  }

  try {
    await Promise.all([
      backend.Alice(ctcA, {
        token: tokenId,
        amt,
        triggerClawback,
        checkBal,
        ...stdlib.hasConsoleLogger,
      })
    ]);
  } catch (e) {
    console.log(e);
    const expFailMethod = `_reachp_${failingMethod}`;
    stdlib.assert(e.toString().includes(expFailMethod),
      `Error was thrown in the expected method: ${expFailMethod}`);
    return;
  }
  stdlib.assert(false, "Expected an error to be thrown when transferring funds that were clawbacked");
}

if (stdlib.connector == 'ALGO') {
  await algo();
} else {
  await eth();
}
