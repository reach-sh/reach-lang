import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';
import algosdk from 'algosdk';
import ethers from 'ethers';
import * as fs from 'fs';

(async () => {
  const stdlib = await stdlib_loader.loadStdlib();
  const conn = stdlib_loader.getConnector();

  const startingBalance = stdlib.parseCurrency(10);
  const myGasLimit = 5000000;

  const ETH_launchToken = async (name, sym) => {
    console.log(`Creator launching ETH token, ${name} (${sym})`);
    const accCreator = await stdlib.newTestAccount(startingBalance);
    accCreator.setGasLimit(myGasLimit);
    const compiled = JSON.parse(await fs.readFileSync('./build/token.sol.json'));
    const remoteCtc = compiled["contracts"]["contracts/token/ERC20/presets/ERC20PresetMinterPauser.sol:ERC20PresetMinterPauser"];
    const remoteABI = remoteCtc["abi"];
    const remoteBytecode = remoteCtc["bin"];
    const factory = new ethers.ContractFactory(remoteABI, remoteBytecode, accCreator.networkAccount);
    console.log(`Creator: deploy`);
    const contract = await factory.deploy(name, sym, { gasLimit: myGasLimit });
    console.log(`Creator: wait for deploy: ${contract.deployTransaction.hash}`);
    const deploy_r = await contract.deployTransaction.wait();
    console.log(`Creator: saw deploy: ${deploy_r.blockNumber}`);
    const id = contract.address;
    console.log(`Creator: deployed: ${id}`);
    const mint = async (accTo, amt) => {
      const to = accTo.networkAccount.address;
      console.log(`Creator: minting ${amt} ${sym} for ${to}`);
      const fn = await contract["mint"](to, amt, { gasLimit: myGasLimit });
      console.log(`Creator: mint: wait`);
      await fn.wait();
    };
    const balanceOf = async (acc) => {
      const addr = acc.networkAccount.address;
      const res = await contract["balanceOf"](addr);
      return res;
    };
    return { name, sym, id, mint, balanceOf };
  };
  const ALGO_launchToken = async (name, sym) => {
    console.log(`${sym} launching ALGO token, ${name} (${sym})`);
    const accCreator = await stdlib.newTestAccount(startingBalance);
    const caddr = accCreator.networkAccount.addr;
    const csk = accCreator.networkAccount.sk;
    const zaddr = caddr;
    // ^ XXX should be nothing; docs say can be "", but doesn't actually work
    console.log(`${sym}: deploy`);
    const algod = await stdlib.getAlgodClient();
    console.log(`${sym}: getParams`);
    const params = await stdlib.getTxnParams();
    console.log(`${sym}: makeAssetCreate`);
    const ctxn = algosdk.makeAssetCreateTxnWithSuggestedParams(
      caddr, undefined, Math.pow(2,48), 6,
      false, zaddr, zaddr, zaddr, zaddr,
      sym, name, '', '', params,
    );
    console.log(`${sym}: signTxn`);
    const ctxn_s = ctxn.signTxn(csk);
    console.log(`${sym}: sendTxn`);
    const ctxn_r = (await algod.sendRawTransaction(ctxn_s).do());
    console.log(`${sym}: waitForConfirm`);
    await stdlib.waitForConfirmation(ctxn_r.txId);
    console.log(`${sym}: lookupAsset`);
    const ctxn_p = await algod.pendingTransactionInformation(ctxn_r.txId).do();
    const id = ctxn_p["asset-index"];
    console.log(`${sym}: asset is ${id}`);

    const mint = async (accTo, amt) => {
      const to = accTo.networkAccount.addr;
      console.log(`${sym}: minting ${amt} ${sym} for ${to}`);
      throw Error(`XXX mint`);
    };
    const balanceOf = async (acc) => {
      const addr = acc.networkAccount.addr;
      console.log(`${sym}: balanceOf of ${addr}`);
      throw Error(`XXX balanceOf`);
    };
    return { name, sym, id, mint, balanceOf };
  };
  const launchTokens = {
    'ETH': ETH_launchToken,
    'ALGO': ALGO_launchToken,
  };
  const launchToken = launchTokens[conn];

  const zorkmid = await launchToken("zorkmid", "ZMD");
  const gil = await launchToken("gil", "GIL");

  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);
  if ( conn === 'ETH' ) {
    accAlice.setGasLimit(myGasLimit);
    accBob.setGasLimit(myGasLimit);
  }

  await zorkmid.mint(accAlice, startingBalance);
  await zorkmid.mint(accAlice, startingBalance);
  await gil.mint(accBob, startingBalance);

  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const doSwap = async (tokenA, amtA, tokenB, amtB) => {
    console.log(`\nPerforming swap of ${fmt(amtA)} ${tokenA.sym} for ${fmt(amtB)} ${tokenB.sym}`);

    console.log(`Alice will deploy the Reach DApp.`);
    const ctcAlice = accAlice.deploy(backend);
    console.log(`Bob attaches to the Reach DApp.`);
    const ctcBob = accBob.attach(backend, ctcAlice.getInfo());

    const getBalance = async (tokenX, who) => {
      const amt = await tokenX.balanceOf(who);
      return `${fmt(amt)} ${tokenX.sym}`; };
    const getBalances = async (who) =>
      `${await getBalance(tokenA, who)} & ${await getBalance(tokenB, who)}`;
    const beforeAlice = await getBalances(accAlice);
    const beforeBob = await getBalances(accBob);

    console.log(`Alice has ${beforeAlice}`);
    console.log(`Bob has ${beforeBob}`);

    await Promise.all([
      backend.Alice(ctcAlice, {
        getSwap: () => {
          console.log(`Alice proposes swap`);
          return [ tokenA.id, amtA, tokenB.id, amtB, 10 ]; },
      }),
      backend.Bob(ctcBob, {
        accSwap: (...v) => {
          console.log(`Bob accepts swap of ${JSON.stringify(v)}`);
          return true; },
      }),
    ]);

    const afterAlice = await getBalances(accAlice);
    const afterBob = await getBalances(accBob);

    console.log(`Alice went from ${beforeAlice} to ${afterAlice}`);
    console.log(`Bob went from ${beforeBob} to ${afterBob}`);
  };

  const amtA = stdlib.parseCurrency(1);
  const amtB = stdlib.parseCurrency(2);

  await doSwap(zorkmid, amtA, gil, amtB);
  await doSwap(gil, amtB, zorkmid, amtA);

  // It would be cool to support ETH without going through WETH
  // const eth = { addr: false, sym: 'ETH', balanceOf: stdlib.balanceOf };
  // await doSwap(eth, amtA, gil, amtB);
  // await doSwap(zorkmid, amtA, eth, amtB);

})();
