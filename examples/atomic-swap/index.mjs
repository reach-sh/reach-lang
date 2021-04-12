import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';
import ethers from 'ethers';
import * as fs from 'fs';

(async () => {
  const stdlib = await stdlib_loader.loadStdlib();

  const startingBalance = stdlib.parseCurrency(10);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const myGasLimit = 5000000;
  accAlice.setGasLimit(myGasLimit);
  accBob.setGasLimit(myGasLimit);

  const launchToken = async (name, sym) => {
    console.log(`Creator launching token, ${name} (${sym})`);
    const accCreator = await stdlib.newTestAccount(startingBalance);
    accCreator.setGasLimit(myGasLimit);
    const compiled = JSON.parse(await fs.readFileSync('./build/token.sol.json'));
    // console.log(`Creator read compiled file: ${JSON.stringify(compiled)}`);
    const remoteCtc = compiled["contracts"]["contracts/token/ERC20/presets/ERC20PresetMinterPauser.sol:ERC20PresetMinterPauser"];
    const remoteABI = remoteCtc["abi"];
    const remoteBytecode = remoteCtc["bin"];
    const factory = new ethers.ContractFactory(remoteABI, remoteBytecode, accAlice.networkAccount);
    console.log(`Creator: deploy`);
    const contract = await factory.deploy(name, sym, { gasLimit: myGasLimit });
    console.log(`Creator: wait for deploy: ${contract.deployTransaction.hash}`);
    const deploy_r = await contract.deployTransaction.wait();
    console.log(`Creator: saw deploy: ${deploy_r.blockNumber}`);
    const addr = contract.address;
    console.log(`Creator: deployed: ${addr}`);
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
      // console.log(`${sym}.balanceOf(${addr}) = ${res}`);
      return res;
    };
    return { sym, addr, mint, balanceOf };
  };

  const zorkmid = await launchToken("zorkmid", "ZMD");
  const gil = await launchToken("gil", "GIL");

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
          return [ tokenA.addr, amtA, tokenB.addr, amtB, 10 ]; },
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
  await doSwap(gil, amtA, zorkmid, amtB);

  // It would be cool to support ETH without going through WETH
  // const eth = { addr: false, sym: 'ETH', balanceOf: stdlib.balanceOf };
  // await doSwap(eth, amtA, gil, amtB);
  // await doSwap(zorkmid, amtA, eth, amtB);

})();
