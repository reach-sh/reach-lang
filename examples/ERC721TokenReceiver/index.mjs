import * as fs from 'fs';
import { loadStdlib } from "@reach-sh/stdlib";
const stdlib = loadStdlib(process.env);
const ethers = stdlib.ethers;

if (stdlib.connector !== "ETH") {
  console.log("This test only applies to ETH");
  process.exit(0);
}

const acc = await stdlib.newTestAccount(stdlib.parseCurrency(100));

// ===== Helper functions =====
const lock = () => {
  const lockObj = {};
  lockObj.reset = () => lockObj.wait = new Promise(r => { lockObj.unlock = r; });
  lockObj.reset();
  return lockObj;
};

const deploy = async (name, abi, bin, args = []) => {
  console.log(`Deploying contract...`, name);
  const factory = new ethers.ContractFactory(abi, bin, acc.networkAccount);
  const contract = await factory.deploy(...args);
  console.log(`Awaiting contract...`, name, contract);
  const r = await contract.deployTransaction.wait();
  console.log(`...waited!`, name, r);
  return contract;
}

const solDeploy = async (solOutputPath, ctcName) => {
  const ctcJson = await fs.promises.readFile(solOutputPath);
  const ctc = JSON.parse(ctcJson)["contracts"][ctcName];
  return deploy('solVersion', ctc.abi, ctc.bin);
}

const rchDeploy = async (rchModulePath, args) => {
  const mod = await import(rchModulePath);
  const ctc = mod._Connectors.ETH;
  return deploy('rchVersion', ctc.ABI, ctc.Bytecode, args);
}

// ===== Launching the contracts =====
// OpenZeppelin based ERC721
const oz_erc721 = await solDeploy("build/oz_erc721.json", "oz_erc721.sol:OZ_ERC721");

// OpenZeppelin based ERC721TokenReceiver
const oz_erc721tr = await solDeploy("build/oz_erc721_tokenreceiver.json",
                                    "oz_erc721_tokenreceiver.sol:OZ_ERC721_TokenReceiver");

// Reach based (dummy) ERC721
const rch_erc721 = await rchDeploy("./build/index.rch_ERC721.mjs",
                                   [[/* time: */ 0, /* zeroAddr: */ "0x" + "0".repeat(40)]]);

// Reach based ERC721TokenReceiver
const rch_erc721tr = await rchDeploy("./build/index.rch_ERC721_TokenReceiver.mjs",
                                     [[/* time: */ 0]]);

// Setup event handlers for GotAToken events
const evHandler = (name, lck) => (operator, from, tokenId, data) => {
  const dataStr = Buffer.from(data.slice(2), "hex").toString("utf8");
  console.log(`${name} GotAToken(${operator}, ${from}, ${tokenId}, "${dataStr}")`);
  lck.unlock([operator, from, tokenId, data]);
}

let oz_gotAToken = lock();
let rch_gotAToken = lock();
oz_erc721tr.on("GotAToken", evHandler("OpenZeppelin", oz_gotAToken));
rch_erc721tr.on("GotAToken", evHandler("Reach", rch_gotAToken));

// ===== Demonstration of ERC721TokenReceiver functionality =====
const assert = stdlib.assert;
const gasLimit = { gasLimit: 5000000 };
const [oz, rch] = [true, false];
const msg = (from, to) =>
  stdlib.stringToHex(`${from ? "OpenZeppelin" : "Reach"} to ${to ? "OpenZeppelin" : "Reach"}`);

let operator, from, tokenId, data;
void(operator, from);

console.log("OpenZeppelin ERC721 to OpenZeppelin ERC721TokenReceiver");
await oz_erc721.mint(oz_erc721tr.address, 123, msg(oz, oz), gasLimit);
[operator, from, tokenId, data] = await oz_gotAToken.wait;
assert(tokenId.eq(123));
assert(data === msg(oz, oz));

console.log("\nOpenZeppelin ERC721 to Reach ERC721TokenReceiver");
await oz_erc721.mint(rch_erc721tr.address, 456, msg(oz, rch), gasLimit);
[operator, from, tokenId, data] = await rch_gotAToken.wait;
assert(tokenId.eq(456));
assert(data === msg(oz, rch));

oz_gotAToken.reset();
rch_gotAToken.reset();

console.log("\nReach ERC721 to OpenZeppelin ERC721TokenReceiver");
await rch_erc721.mint(oz_erc721tr.address, 42, msg(rch, oz), gasLimit);
[operator, from, tokenId, data] = await oz_gotAToken.wait;
assert(tokenId.eq(42));
assert(data === msg(rch, oz));

console.log("\nReach ERC721 to Reach ERC721TokenReceiver");
await rch_erc721.mint(rch_erc721tr.address, 5318008, msg(rch, rch), gasLimit);
[operator, from, tokenId, data] = await rch_gotAToken.wait;
assert(tokenId.eq(5318008));
assert(data === msg(rch, rch));

process.exit(0);
