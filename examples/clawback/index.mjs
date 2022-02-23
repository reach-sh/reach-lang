import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib(process.env);
const pc = stdlib.parseCurrency;
const b = pc(100);
const balOf = async (acc, tok) => stdlib.balanceOf(acc, tok);


const algo = async (accAlice, ctcA, token) => {
  const { algosdk } = stdlib;
  const tokenId = token.id.toNumber();
  const amt = pc(20);

  const requestMoney = async (addr) => {
    console.log(`Sending ${amt.toString()} to ${addr}`)
    const address = stdlib.formatAddress(addr);
    await stdlib.transfer(accAlice, address, amt, tokenId);
  }

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
        2: pc(0),
      }[idx];
    stdlib.assert(bal.eq(expectedBal), "Expected correct balance");
    console.log(`Balance:`, bal.toString());
  }

  await Promise.all([
    backend.Alice(ctcA, {
      token: tokenId,
      requestMoney,
      triggerClawback,
      checkBal,
      ...stdlib.hasConsoleLogger,
    })
  ]);
}

const eth = async (accAlice, ctcA, token) => {
  const tokenId = token.id;

  const requestMoney = async (addr) => {
    console.log(`Request money`);
    const address = stdlib.formatAddress(addr);
    await stdlib.transfer(accAlice, address, pc(20), tokenId);
  }

  const triggerClawback = async (addr, funds) => {
    console.log(`Clawback`);
  }

  const checkBal = async (addr) => {
    console.log(`checkBal`);
    // const bal = await balOf(addr, tokenId);
    const bal = pc(0);
    console.log(`Balance:`, bal.toString());
  }

  await Promise.all([
    backend.Alice(ctcA, {
      token: tokenId,
      requestMoney,
      triggerClawback,
      checkBal,
      ...stdlib.hasConsoleLogger,
    })
  ]);
}

(async () => {

  const accAlice = await stdlib.newTestAccount(b);
  const ctcA = accAlice.contract(backend);

  const token = await stdlib.launchToken(accAlice, "Zorkmid", "ZMD", { clawback: accAlice });
  console.log(`Launched token:`, token.id.toString());

  if (stdlib.connector == 'ALGO') {
    // Fails with:
    //    logic eval error: invalid Asset reference 18233"
    // I believe token needs to be specified as an offset in the foreign assets array.
    // See: https://forum.algorand.org/t/asset-holding-get-doesnt-recognize-asset-id/3790
    await algo(accAlice, ctcA, token);
  } else {
    await eth(accAlice, ctcA, token);
  }
})();
