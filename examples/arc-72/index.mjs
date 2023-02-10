import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);
const assert = stdlib.assert;

const assertFail = async (promise, errStr) => {
  try {
    await promise;
  } catch (e) {
    if (errStr) {
      if (`${e}`.includes(errStr)) {
        return;
      }
      throw `Expected exception including message: "${errStr}", but got: ${e}`;
    } else {
      return;
    }
  }
  throw `Expected exception but did not catch one: ${errStr}`;
};
const assertEq = (a, b, context = 'assertEq') => {
  if (a === b) return;
  try {
    const res1BN = bigNumberify(a);
    const res2BN = bigNumberify(b);
    if (res1BN.eq(res2BN)) return;
  } catch {}
  try {
    const stripNulls = (s) => s.replace(/\0*$/g, "");
    if (stripNulls(`${a}`) === stripNulls(`${b}`)) return;
  } catch {}
  try {
    if (JSON.stringify(a) === JSON.stringify(b)) return;
  } catch {}
  try {
    if (parseInt(a) == parseInt(b)) return;
  } catch {}
  assert(false, `${context}: ${a} == ${b}`);
};

const initBal = stdlib.parseCurrency(100);
const accs = await stdlib.newTestAccounts(4, initBal);
accs.forEach(acc => acc.setGasLimit(5000000));
const [acc0, acc1, acc2, acc3] = accs;
const [addr0, addr1, addr2, addr3] = accs.map(a => a.getAddress());

const zeroAddress = "0x" + "0".repeat(40);

const ctc0 = acc0.contract(backend);
const params = {
  zeroAddress,
  // This is the length of an ipfs base32 cid v1
  metadataUriBase: "ipfs://bafy0000000000000000000000000000000000000000000000000000000/",
};
console.log("About to launch NFT contract.");
await stdlib.withDisconnect(() => ctc0.participants.Deployer({
  params,
  ready: () => {stdlib.disconnect()}
}));
const ctcinfo = await ctc0.getInfo();
const ctc = acc => acc.contract(backend, ctcinfo);
const [ctc1, ctc2, ctc3] = [acc1, acc2, acc3].map(a => ctc(a));

const assertEvent = async (event, ...expectedArgs) => {
  const e = await ctc0.events[event].next();
  const actualArgs = e.what;
  expectedArgs.forEach((expectedArg, i) =>
    assertEq(actualArgs[i], expectedArg, `${event} field ${i}`)
  );
};

const assertView = async (view, args, expectedRet) => {
  const ret = await ctc0.unsafeViews[view](...args);
  assertEq(
    ret,
    expectedRet,
    `view ${view}: expected ${expectedRet}, got ${ret}`
  );
};

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Test happy/sad paths

console.log("About to start testing.");
// Interface ID that must be false
await assertView("supportsInterface", ["0xffffffff"], false);
// supportsInterface (ARC-73) interface ID
await assertView("supportsInterface", ["0x4e22a3ba"], true);
// ARC-72 core id
await assertView("supportsInterface", ["0x15974096"], true);
// ARC-72 metadata id
await assertView("supportsInterface", ["0x9112544c"], true);
// ARC-72 transfer management id
await assertView("supportsInterface", ["0x924d64fb"], true);
// Some non-existent interface ID
await assertView("supportsInterface", ["0x12345678"], false);

await assertView("ownerOf", [1], zeroAddress);
await assertFail(ctc1.a.mintTo(addr2), "must be admin");
await assertFail(ctc2.a.transferFrom(addr2, addr3, 1), "nft must exist");

console.log("About to mint first NFT.");
await ctc0.a.mintTo(addr2);
await assertEvent("Transfer", zeroAddress, addr2, 1);
await assertView("ownerOf", [1], addr2)
await assertView("tokenURI", [1], params.metadataUriBase + "0001");
await assertFail(ctc0.a.transferFrom(addr2, addr3, 1), "must be nft owner or approved operator");
await assertFail(ctc0.a.transferFrom(addr0, addr3, 1), "owner specified in API must be correct");
await ctc2.a.transferFrom(addr2, addr3, 1);
await assertEvent("Transfer", addr2, addr3, 1);
await assertView("ownerOf", [1], addr3)
await assertFail(ctc0.a.transferFrom(addr3, addr1, 1), "must be nft owner or approved operator");
await assertFail(ctc0.a.transferFrom(addr0, addr1, 1), "owner specified in API must be correct");
await ctc3.a.transferFrom(addr3, addr2, 1);
await assertEvent("Transfer", addr3, addr2, 1);
await assertView("ownerOf", [1], addr2)

console.log("About to test transfer management.");
await ctc0.a.mintTo(addr2);
await assertEvent("Transfer", zeroAddress, addr2, 2);
await assertView("ownerOf", [2], addr2)
await assertView("tokenURI", [2], params.metadataUriBase + "0002");
await assertView("getApproved", [2], zeroAddress);
await assertView("isApprovedForAll", [addr3, addr1], false);
await ctc3.a.setApprovalForAll(addr1, true);
await assertEvent("ApprovalForAll", addr3, addr1, true);
await assertView("isApprovedForAll", [addr3, addr1], true);
await assertFail(ctc0.a.transferFrom(addr2, addr3, 2), "must be nft owner or approved operator");
await ctc2.a.approve(addr0, 2);
await assertView("getApproved", [2], addr0);
await ctc0.a.transferFrom(addr2, addr3, 2);
await assertEvent("Transfer", addr2, addr3, 2);
await assertView("getApproved", [2], zeroAddress);
// addr1 is approved for all for addr3
await ctc1.a.transferFrom(addr3, addr1, 2);
await assertEvent("Transfer", addr3, addr1, 2);
await assertView("isApprovedForAll", [addr3, addr1], true);
await ctc1.a.transferFrom(addr1, addr3, 2);
await assertEvent("Transfer", addr1, addr3, 2);
await ctc3.a.setApprovalForAll(addr1, false);
await assertEvent("ApprovalForAll", addr3, addr1, false);
await assertView("isApprovedForAll", [addr3, addr1], false);
// addr1 is no longer approved
await assertFail(ctc1.a.transferFrom(addr3, addr1, 2));

await assertView("totalSupply", [], 2);
await assertFail(ctc1.a.burn(2), "must be nft owner or approved operator");
await ctc3.a.burn(2);
await assertEvent("Transfer", addr3, zeroAddress, 2);
await assertView("totalSupply", [], 1);

console.log("About to test admin update.");
await assertFail(ctc1.a.updateAdmin(addr2), "must be admin");
await ctc0.a.updateAdmin(addr2);
await assertFail(ctc0.a.updateAdmin(addr3), "must be admin");
await assertFail(ctc0.a.mintTo(addr3), "must be admin");
await ctc2.a.mintTo(addr3)
await assertEvent("Transfer", zeroAddress, addr3, 3);
await assertView("ownerOf", [3], addr3)
await assertView("totalSupply", [], 2);


console.log("Done testing NFT contract.");

