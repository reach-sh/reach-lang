import * as fs from 'fs';
import { loadStdlib, ask } from "@reach-sh/stdlib";
const stdlib = loadStdlib(process.env);
const ethers = stdlib.ethers;

if (stdlib.connector !== "ETH") {
  console.log("This test only applies to ETH");
  process.exit(0);
}

const accs = await stdlib.newTestAccounts(4, stdlib.parseCurrency(100));
//const acc = await stdlib.newAccountFromMnemonic(await ask.ask("input secret:", (x => x))); const accs = [acc, acc, acc, acc];
const [accDeploy, acc1, acc2, acc3] = accs;
const [addrDeploy, addr1, addr2, addr3] = accs.map(a => a.getAddress());
const [tok1, tok2, tok3, tok4, tok5] = [1, 2, 3, 4, 5];
const gasLimit = { gasLimit: 5_000_000 };
const zeroAddr = "0x" + "0".repeat(40);
const assert = stdlib.assert;
const bigNumberify = ethers.BigNumber.from;
const waitTxn = async callPromise => await (await callPromise).wait();

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

const deploy = async (name, abi, bin, args = []) => {
  console.log("Deploying...", name)
  const factory = new ethers.ContractFactory(abi, bin, accDeploy.networkAccount);
  const contract = await factory.deploy(...args);
  console.log('Waiting', name, contract)
  const txn = await contract.deployTransaction.wait();
  console.log('Done waiting', name, txn)
  return [contract, txn.gasUsed];
}

const solDeploy = async (solOutputPath, ctcName, args = []) => {
  const ctcJson = await fs.promises.readFile(solOutputPath);
  const ctc = JSON.parse(ctcJson)["contracts"][ctcName];
  return deploy('sol', ctc.abi, ctc.bin, args);
}

const rchDeploy = async (rchModulePath, args) => {
  const mod = await import(rchModulePath);
  const ctc = mod._Connectors.ETH;
  return deploy('reach', ctc.ABI, ctc.Bytecode, args);
}

const deployReceiver = async () => {
  const [receiverCtc, receiverCtcGasUsed]
        = await rchDeploy("./build/index.testTokenReceiver.mjs", [[0]]);
  return receiverCtc;
}

const test = async (ctc, expected, testEnumerable) => {
  console.log(`Testing ${expected.name}`);
  const getWei = async () => (await accDeploy.balanceOf()).add(await acc1.balanceOf()).add(await acc2.balanceOf()).add(await acc3.balanceOf());
  const weiPre = await getWei();

  // ===== ERC165 =====
  const interfaceIds = {
    ERC165: "0x01ffc9a7",
    ERC721: "0x80ac58cd",
    ERC721Metadata: "0x5b5e139f",
    ...(testEnumerable ? {ERC721Enumerable: "0x780e9d63"} : {})
  };

  for (const iface in interfaceIds) {
    assert(await ctc.supportsInterface(interfaceIds[iface]), `Supports ${iface}`);
  }

  // ===== ERC721 =====
  // add event listeners
  const evLocks = {};
  const assertEvent = {};
  for (const ev of ["Transfer", "Approval", "ApprovalForAll"]) {
    // Make lock, to wait for an event to occur
    const l = lock();
    ctc.on(ev, (...args) => l.unlock(args));
    evLocks[ev] = l;

    // Event occurrence assertion helper
    assertEvent[ev] = async (...expectedArgs) => {
      const args = await l.wait;
      l.reset();
      expectedArgs.forEach((expectedArg, i) => assertEq(args[i], expectedArg, `${ev} field ${i}`));
    }
  }

  // A few other helpers
  const assertOwners = async (...owners) => {
    assertEq(await ctc.ownerOf(tok1), owners[0], "ownerOf(1)");
    assertEq(await ctc.ownerOf(tok2), owners[1], "ownerOf(2)");
    assertEq(await ctc.ownerOf(tok3), owners[2], "ownerOf(3)");

    const countAddr = addr => owners.reduce((n, a) => n + (addr === a ? 1 : 0), 0);
    assertEq(await ctc.balanceOf(addr1), countAddr(addr1), "balanceOf(addr1)");
    assertEq(await ctc.balanceOf(addr2), countAddr(addr2), "balanceOf(addr2)");
    assertEq(await ctc.balanceOf(addr3), countAddr(addr3), "balanceOf(addr3)");
  };
  const [_, ctc1, ctc2, ctc3] = accs.map(a => ctc.connect(a.networkAccount));
  const forEachTok = f => f(tok1).then(_ => f(tok2)).then(_ => f(tok3));
  const mkTransfer = transferFn => async (from, to, tok, ctcOverride) => {
    const fromCtc = ctcOverride ?? (from === addr1 ? ctc1 : (from === addr2 ? ctc2 : ctc3));
    await waitTxn(fromCtc[transferFn](from, to, tok, gasLimit));
    await assertEvent.Approval(from, zeroAddr, tok);
    await assertEvent.Transfer(from, to, tok);
  };
  const transferFrom = mkTransfer("transferFrom");
  const safeTransferFrom = mkTransfer("safeTransferFrom(address,address,uint256)");

  // zero addr balance should throw
  await assertFail(ctc.balanceOf(zeroAddr));

  // Tokens not minted yet should throw
  await forEachTok(t => assertFail(ctc.ownerOf(t)));

  // A minting method is not specified in ERC721, so we are just expecting
  // a method "mint" to exist on the contract. (It IS specified that minting
  // produces a Transfer event from the zero addr)
  await forEachTok(t => waitTxn(ctc.mint(addr1, t, gasLimit))
                   .then(_ => assertEvent.Transfer(zeroAddr, addr1, t))
                  );
  await assertOwners(addr1, addr1, addr1);

  // non-owner transfer should fail
  await forEachTok(t => assertFail(safeTransferFrom(addr2, addr1, t)));
  await forEachTok(t => assertFail(transferFrom(addr2, addr1, t)));

  // transfer signed by owner but _from is not the owner should fail
  await forEachTok(t => assertFail(safeTransferFrom(addr3, addr2, t, ctc1)));
  await forEachTok(t => assertFail(transferFrom(addr3, addr2, t, ctc1)));

  // transfer to zero addr should fail
  await forEachTok(t => assertFail(safeTransferFrom(addr1, zeroAddr, t)));
  await forEachTok(t => assertFail(transferFrom(addr1, zeroAddr, t)));


  // transfer all tokens from addr1 to addr2 using safeTransferFrom
  await safeTransferFrom(addr1, addr2, tok1);
  await assertOwners(addr2, addr1, addr1);
  await safeTransferFrom(addr1, addr2, tok2);
  await assertOwners(addr2, addr2, addr1);
  await safeTransferFrom(addr1, addr2, tok3);
  await assertOwners(addr2, addr2, addr2);


  // transfer all tokens from addr2 to addr1 using transferFrom
  await transferFrom(addr2, addr1, tok1);
  await assertOwners(addr1, addr2, addr2);
  await transferFrom(addr2, addr1, tok2);
  await assertOwners(addr1, addr1, addr2);
  await transferFrom(addr2, addr1, tok3);
  await assertOwners(addr1, addr1, addr1);

  // getApproved should return zero addr if nobody is approved
  await forEachTok(async t => assertEq(await ctc.getApproved(t), zeroAddr, "getApproved"));

  // approve addr2 to operate tok1/2/3 for addr1
  await forEachTok(t => waitTxn(ctc1.approve(addr2, t, gasLimit))
                          .then(_ => assertEvent.Approval(addr1, addr2, t)));

  // "reaffirmed" approval of addr2 should still emits events
  await forEachTok(t => waitTxn(ctc1.approve(addr2, t, gasLimit))
                          .then(_ => assertEvent.Approval(addr1, addr2, t)));

  // Approved addr cannot approve someone else
  await forEachTok(t => assertFail(waitTxn(ctc2.approve(addr3, t, gasLimit))));

  // Rando cannot approve someone
  await forEachTok(t => assertFail(waitTxn(ctc3.approve(addr3, t, gasLimit))));

  // Transfer all toks to addr3 using approved ctc2
  await assertOwners(addr1, addr1, addr1);
  await forEachTok(t => safeTransferFrom(addr1, addr3, t, ctc2));
  await assertOwners(addr3, addr3, addr3);

  // Approve addr1 as an operator for addr3
  assert(!(await ctc.isApprovedForAll(addr3, addr1)));
  await waitTxn(ctc3.setApprovalForAll(addr1, true));
  assert(await ctc.isApprovedForAll(addr3, addr1));

  // Operator can approve others
  await waitTxn(ctc1.approve(addr2, tok1));
  await assertEvent.Approval(addr3, addr2, tok1);

  // Operator can transfer
  await safeTransferFrom(addr3, addr2, tok1, ctc2);

  // TODO - the Reach implementation does not yet support ERC721Enumerable
  if (testEnumerable){
    // ===== ERC721Enumerable =====
    assertEq(await ctc.totalSupply(), 3, "totalSupply");

    // addr1 has no tokens
    await assertFail(ctc.tokenOfOwnerByIndex(addr1, 0));

    // addr2 has 1 token
    assertEq(await ctc.tokenOfOwnerByIndex(addr2, 0), tok1, "tokenOfOwnerByIndex(addr2)");

    // addr3 has 2 tokens
    let seen = {};
    seen[await ctc.tokenOfOwnerByIndex(addr3, 0)] = true;
    seen[await ctc.tokenOfOwnerByIndex(addr3, 1)] = true;
    assert(seen[tok2] && seen[tok3], "tokenOfOwnerByIndex(addr3)");

    // 3 total tokens exist, can be found with tokenByIndex
    seen = {};
    for (let i = 0; i < 3; i++) {
      const tokId = await ctc.tokenByIndex(i);
      seen[tokId] = true;
    }
    assert(seen[tok1] && seen[tok2] && seen[tok3], "tokenByIndex");

    // index >= totalSupply
    await assertFail(ctc.tokenByIndex(3));
    await assertFail(ctc.tokenByIndex(100));
  }

  // ===== ERC721Metadata =====
  assertEq(await ctc.name(), expected.name, "name()");
  assertEq(await ctc.symbol(), expected.symbol, "symbol()");
  await forEachTok(async t => assertEq(await ctc.tokenURI(t), expected.tokenURIs[t], "tokenURI"));

  // invalid token id has no uri
  await assertFail(ctc.tokenURI(4));

  // Test safeTransferFrom to a contract
  const receiverCtc = await deployReceiver()
  await safeTransferFrom(addr2, receiverCtc.address, tok1);
  await safeTransferFrom(addr3, receiverCtc.address, tok2);
  await safeTransferFrom(addr3, receiverCtc.address, tok3);
  await assertOwners(receiverCtc.address, receiverCtc.address, receiverCtc.address);
  await receiverCtc.transfer(ctc.address, addr1, tok1, []);
  await assertOwners(addr1, receiverCtc.address, receiverCtc.address);
  await receiverCtc.transfer(ctc.address, addr1, tok2, []);
  await receiverCtc.transfer(ctc.address, addr1, tok3, []);
  await assertOwners(addr1, addr1, addr1);

  const weiPost = await getWei();
  const weiDiff = weiPre.sub(weiPost);
  return weiDiff;
};

// OpenZeppelin based ERC721
const ozERC721Deploy = async () => {
  return await solDeploy("build/oz_erc721.json", "oz_erc721.sol:OZ_ERC721");
}
const oz_erc721_expected = {
  name: "OZ_ERC721",
  symbol: "OZ",
  tokenURIs: {
    [tok1]: "OZ_ERC721/1",
    [tok2]: "OZ_ERC721/2",
    [tok3]: "OZ_ERC721/3",
    [tok4]: "OZ_ERC721/4",
    [tok5]: "OZ_ERC721/5",
  },
};


//// Test Reach based ERC721

// Launch the Reach contract in the same way that the OpenZeppelin contract was launched...
const reach_erc721_constructor_args = [
  [
    // time
    0,
    // v3236, string, name
    "Reach_ERC721",
    // v3237, string, symbol
    "RCH",
    // v3238, string, tokenURI
    "Reach_ERC721/",
    // v3239, uint256, I think this is totalSupply
    5,
    // v3240, address payable, I think this is the zero address
    zeroAddr,
    // Empty BytesDyn
    [],
  ],
];
const reachERC721Deploy = async () => {
  return await rchDeploy("./build/index.main.mjs", reach_erc721_constructor_args);
}
const reach_erc721_expected = {
  name: "Reach_ERC721",
  symbol: "RCH",
  tokenURIs: {
    [tok1]: "Reach_ERC721/1",
    [tok2]: "Reach_ERC721/2",
    [tok3]: "Reach_ERC721/3",
  },
};



// Actually run the tests, side by side.
const ozCost = await test((await ozERC721Deploy())[0], oz_erc721_expected, false);
const reachCost = await test((await reachERC721Deploy())[0], reach_erc721_expected, false);

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

  card["mint_1"] = await g(accDeploy, "mint", addr1, 1);
  card["mint_2"] = await g(accDeploy, "mint", addr1, 2);
  card["mint_3"] = await g(accDeploy, "mint", addr1, 3);
  card["mint_4"] = await g(accDeploy, "mint", addr1, 4);
  card["mint_5"] = await g(accDeploy, "mint", addr1, 5);

  card["transferFrom"] = await g(acc1, "transferFrom", addr1, addr2, 1);

  card["safeTransferFrom_noBytes_eoa"] = await g(acc2, "safeTransferFrom(address,address,uint256)", addr2, addr1, 1);
  card["safeTransferFrom_bytes_eoa"] = await g(acc1, "safeTransferFrom(address,address,uint256,bytes)", addr1, addr2, 1, []);

  const receiverCtc = await deployReceiver();
  card["safeTransferFrom_bytes_ctc"] = await g(acc2, "safeTransferFrom(address,address,uint256,bytes)", addr2, receiverCtc.address, 1, []);
  await receiverCtc.transfer(ctc.address, addr2, 1, [], gasLimit);
  card["safeTransferFrom_noBytes_ctc"] = await g(acc2, "safeTransferFrom(address,address,uint256)", addr2, receiverCtc.address, 1);
  await receiverCtc.transfer(ctc.address, addr2, 1, [], gasLimit);

  card["burn"] = await g(acc1, "burn", 3);

  if(card["burn"] !== "N/A"){
    // Test that re-minting after burning fails.
    await assertFail(g(accDeploy, "mint", addr1, 3));
  }


  return card;
}
const ozBenchCard = await bench(ozERC721Deploy);
console.log("OpenZeppelin costs: ", ozBenchCard);
const reachBenchCard = await bench(reachERC721Deploy);
console.log("Reach costs: ", reachBenchCard);



process.exit(0);

// Computs ERC165 interface ID from array of method signatures
// const interfaceId = (methods) => {
//   let a = null;
//   for (const f of methods) {
//     const sel = ethers.utils.arrayify(ethers.utils.keccak256(ethers.utils.toUtf8Bytes(f))).slice(0,4);
//     if (a) {
//       for (let i = 0; i < 4; i++) {
//         a[i] = a[i] ^ sel[i];
//       }
//     } else {
//       a = sel;
//     }
//   }
//   return a ? ethers.utils.hexlify(a) : null;
// }
