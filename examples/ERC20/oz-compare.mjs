import * as fs from 'fs';
import { loadStdlib, ask } from "@reach-sh/stdlib";
const stdlib = loadStdlib(process.env);
const ethers = stdlib.ethers;

export const ozComparisonTests = async () => {
  if (stdlib.connector !== "ETH") {
    console.log("This test only applies to ETH");
    return;
  }

  const accs = await stdlib.newTestAccounts(4, stdlib.parseCurrency(100));
  //const acc = await stdlib.newAccountFromMnemonic(await ask.ask("input secret:", (x => x))); const accs = [acc, acc, acc, acc];
  const [accDeploy, acc1, acc2, acc3] = accs;
  const [addrDeploy, addr1, addr2, addr3] = accs.map(a => a.getAddress());
  const [tok1, tok2, tok3, tok4, tok5] = [1, 2, 3, 4, 5];
  const gasLimit = { gasLimit: 5_000_000 };
  const zeroAddress = "0x" + "0".repeat(40);
  const assert = stdlib.assert;
  const bigNumberify = ethers.BigNumber.from;
  const waitTxn = async callPromise => await (await callPromise).wait();

  const decimals = 2;
  const totalSupply = 1000_00;

  console.log("addrDeploy =", addrDeploy);
  console.log("addr1 =", addr1);
  console.log("addr2 =", addr2);
  console.log("addr3 =", addr3);

  const assertFail = async (promise) => {
    try {
      await promise;
    } catch (e) {
      return;
    }
    throw "Expected exception but did not catch one";
  }

  const assertEq = (a, b, context = "assertEq") => {
    if (a === b) return;
    try {
      const res1BN = bigNumberify(a);
      const res2BN = bigNumberify(b);
      if (res1BN.eq(res2BN)) return;
    } catch {}
    assert(false, `${context}: ${a} == ${b}`);
  }

  const lock = () => {
    const lockObj = {};
    lockObj.reset = () => lockObj.wait = new Promise(r => { lockObj.unlock = r; });
    lockObj.reset();
    return lockObj;
  };

  const deploy = async (abi, bin, args = []) => {
    console.log("Deploying...")
    const factory = new ethers.ContractFactory(abi, bin, accDeploy.networkAccount);
    const contract = await factory.deploy(...args);
    const txn = await contract.deployTransaction.wait();
    return [contract, txn.gasUsed];
  }

  const solDeploy = async (solOutputPath, ctcName, args = []) => {
    const ctcJson = await fs.promises.readFile(solOutputPath);
    const ctc = JSON.parse(ctcJson)["contracts"][ctcName];
    return deploy(ctc.abi, ctc.bin, args);
  }

  const rchDeploy = async (rchModulePath, args) => {
    const mod = await import(rchModulePath);
    const ctc = mod._Connectors.ETH;
    return deploy(ctc.ABI, ctc.Bytecode, args);
  }

  // ===== ERC165 =====
  const interfaceIds = {
    ERC165: "0x01ffc9a7",
    ERC20: "0x36372b07",
  };

  const test = async (ctc, expected) => {
    console.log(`Testing ${expected.name}`);
    const getWei = async () => (await accDeploy.balanceOf()).add(await acc1.balanceOf()).add(await acc2.balanceOf()).add(await acc3.balanceOf());
    const weiPre = await getWei();

    // ===== ERC20 =====
    // add event listeners
    const evLocks = {};
    const assertEvent = {};
    for (const ev of ["Transfer", "Approval"]) {
      // Make lock, to wait for an event to occur
      const l = lock();
      ctc.on(ev, (...args) => l.unlock(args));
      evLocks[ev] = l;

      // Event occurrence assertion helper
      assertEvent[ev] = async (...expectedArgs) => {
        const args = await l.wait;
        //console.log("Assert event ", ev, ", expected args: ", expectedArgs)
        //console.log("Assert event", ev, ", actual args: ", args.slice(0, expectedArgs.length))
        l.reset();
        expectedArgs.forEach((expectedArg, i) => assertEq(args[i], expectedArg, `${ev} field ${i}`));
      }
    }

    await assertEvent.Transfer(zeroAddress, addrDeploy, totalSupply);

    const assertBalances = async (...balances) => {
      assertEq(await ctc.balanceOf(addrDeploy), balances[0]);
      assertEq(await ctc.balanceOf(addr1), balances[1]);
      assertEq(await ctc.balanceOf(addr2), balances[2]);
      assertEq(await ctc.balanceOf(addr3), balances[3]);
    }

    const [_, ctc1, ctc2, ctc3] = accs.map(a => ctc.connect(a.networkAccount));

    const transfer = async (uctc, to, amt) => {
      await waitTxn(uctc.transfer(to, amt, gasLimit));
      await assertEvent.Transfer(uctc.signer.address, to, amt);
    }
    const transferFrom = async (uctc, from, to, amt, allowanceLeft) => {
      await waitTxn(uctc.transferFrom(from, to, amt, gasLimit));
      await assertEvent.Transfer(from, to, amt);
      await assertEvent.Approval(from, uctc.signer.address, allowanceLeft);
    }
    const approve = async (uctc, spender, amt) => {
      await waitTxn(uctc.approve(spender, amt, gasLimit));
      await assertEvent.Approval(uctc.signer.address, spender, amt);
    }

    await assertBalances(totalSupply, 0, 0, 0);

    // transfer of more than you have should fail
    await assertFail(transfer(ctc1, addr2, 10));
    await assertFail(transferFrom(ctc1, addr2, addr3, 10, 0));
    // transfer of zero should work even if you don't have any, based on my reading of the spec
    await transfer(ctc1, addr2, 0);
    // transferFrom of zero should work even the from doesn't have any and the transferer has an allowance of 0, based on my reading of the spec.
    await transferFrom(ctc1, addr2, addr3, 0, 0);

    await transfer(ctc, addr1, 10);
    await assertBalances(totalSupply - 10, 10, 0, 0);
    assertEq(await ctc.allowance(addrDeploy, addr3), 0);
    await approve(ctc, addr3, 20);
    assertEq(await ctc.allowance(addrDeploy, addr3), 20);
    await assertBalances(totalSupply - 10, 10, 0, 0);
    // transferFrom of more than an allowance should fail
    await assertFail(transferFrom(ctc3, addrDeploy, addr2, 100, 20));
    await transferFrom(ctc3, addrDeploy, addr2, 10, 10);
    assertEq(await ctc.allowance(addrDeploy, addr3), 10);
    await assertBalances(totalSupply - 20, 10, 10, 0);
    await transferFrom(ctc3, addrDeploy, addr3, 10, 0);
    assertEq(await ctc.allowance(addrDeploy, addr3), 0);
    await assertBalances(totalSupply - 30, 10, 10, 10);
    // transferFrom should use up the allowance
    await assertFail(transferFrom(ctc3, addrDeploy, addr3, 1, 0));

    // Even if you're rich, you can't transfer more than your balance.
    await assertFail(transfer(ctc, addr2, totalSupply - 10));

    // Can't transfer to zero address
    await assertFail(transfer(ctc, zeroAddress, 10));
    // Can't approve zero address
    await assertFail(approve(ctc, zeroAddress, 10));

    await approve(ctc, addr1, 100);
    // Can't transferFrom to the zero address
    await assertFail(transferFrom(ctc1, ctc, zeroAddress, 10));

    assertEq(await ctc.name(), expected.name, "name()");
    assertEq(await ctc.symbol(), expected.symbol, "symbol()");
    assertEq(await ctc.totalSupply(), expected.totalSupply, "totalSupply()");
    assertEq(await ctc.decimals(), expected.decimals, "decimals()");

    const weiPost = await getWei();
    const weiDiff = weiPre.sub(weiPost);
    return weiDiff;
  };

  // OpenZeppelin based ERC20
  const oz_expected = {
    name: "OZ_ERC20",
    symbol: "OZ",
    decimals: decimals,
    totalSupply: totalSupply,
  };
  const ozDeploy = async () => {
    const oe = oz_expected;
    return await solDeploy("build/oz_erc20.json", "oz_erc20.sol:OZ_ERC20",
                           [oe.name, oe.symbol, oe.decimals, oe.totalSupply]);
  }

  // Reach based ERC20
  const reach_meta = {
    name: "Reach_ERC20",
    symbol: "RCH",
    decimals,
    totalSupply,
    zeroAddress,
  }
  const reach_constructor_args = [
    [
      // time
      0,
      reach_meta.name,
      reach_meta.symbol,
      reach_meta.decimals,
      reach_meta.totalSupply,
      reach_meta.zeroAddress
    ],
  ];
  const reachDeploy = async () => {
    return await rchDeploy("./build/index.main.mjs", reach_constructor_args);
  }
  const reach_expected = {
    name: "Reach_ERC20",
    symbol: "RCH",
    decimals, totalSupply
  };



  // Actually run the tests, side by side.
  const reachCost = await test((await reachDeploy())[0], reach_expected);
  const ozCost = await test((await ozDeploy())[0], oz_expected);

  console.log("Cost of Reach contract as percentage of OZ contract (for a test suite run): ", reachCost.mul(100_000.0).div(ozCost).toNumber() / 1000);


  // More granular gas benchmark
  const bench = async (deployFunc) => {
    const card = {};
    const [ctc, deployGas] = await deployFunc();
    card["Deploy"] = deployGas.toNumber();

    const g = async (acc, f, ...args) => {
      //console.log("calling: ", f, "  with addr: ", await acc.getAddress())
      const cWithAddr = ctc.connect(acc.networkAccount);
      const fn = cWithAddr[f];
      if(fn) {
        return (await (await fn(...args, gasLimit)).wait()).gasUsed.toNumber();
      } else {
        return "N/A";
      }
    }

    card["transfer_0"] = await g(accDeploy, "transfer", addr1, 0);
    card["transfer_1"] = await g(acc2, "transfer", addr1, 0);
    card["transfer_2"] = await g(accDeploy, "transfer", addr1, 20);
    card["transfer_3"] = await g(accDeploy, "transfer", addr1, 30);
    card["transfer_4"] = await g(accDeploy, "transfer", addr1, 40);
    card["transfer_5"] = await g(accDeploy, "transfer", addr1, 50);

    card["approval_1"] = await g(accDeploy, "approve", addr3, 0);
    card["approval_2"] = await g(accDeploy, "approve", addr3, 50);

    card["transferFrom_0"] = await g(acc2, "transferFrom", addrDeploy, addr2, 0);
    card["transferFrom_1"] = await g(acc3, "transferFrom", addrDeploy, addr1, 10);


    return card;
  }
  const ozBenchCard = await bench(ozDeploy);
  console.log("OpenZeppelin costs: ", ozBenchCard);
  const reachBenchCard = await bench(reachDeploy);
  console.log("Reach costs: ", reachBenchCard);


  {
    // Now do some tests that are specific to the Reach ERC20 and not the OZ ERC20
    const backend = await import('./build/index.main.mjs');

    const ctc = accDeploy.contract(backend);

    try {
      await ctc.p.Deployer({meta: reach_meta, deployed: (c) => {throw 42}});
    } catch (e) {
      if ( e !== 42) {
        throw e;
      }
    }

    for (const iface in interfaceIds) {
      assert(await ctc.views.supportsInterface(interfaceIds[iface]), `Supports ${iface}`);
    }
  }
}
