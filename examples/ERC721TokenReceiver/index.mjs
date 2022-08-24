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
  let unlock, wait;
  const reset = () => wait = new Promise(r => { unlock = r; });
  reset();
  return { unlock, wait, reset };
};

const deploy = (abi, bin, args = []) =>
  (new ethers.ContractFactory(abi, bin, acc.networkAccount)).deploy(...args);

const solDeploy = async (solOutputPath, ctcName) => {
  const ctcJson = await fs.promises.readFile(solOutputPath);
  const ctc = JSON.parse(ctcJson)["contracts"][ctcName];
  return deploy(ctc.abi, ctc.bin);
}

const rchDeploy = async (rchModulePath, args) => {
  const mod = await import(rchModulePath);
  const ctc = mod._Connectors.ETH;
  return deploy(ctc.ABI, ctc.Bytecode, args);
}

// ===== Launching the contracts =====
// OpenZeppelin based ERC721
const oz_erc721 = await solDeploy("build/oz_erc721.json", "oz_erc721.sol:OZ_ERC721");

// OpenZeppelin based ERC721TokenReceiver
const oz_erc721tr = await solDeploy("build/oz_erc721_tokenreceiver.json",
                                    "oz_erc721_tokenreceiver.sol:OZ_ERC721_TokenReceiver");

// Reach based ERC721TokenReceiver
const rch_erc721tr = await rchDeploy("./build/index.rch_ERC721_TokenReceiver.mjs",
                                     [[/* time: */0, [[ /* selector: */ "0x150b7a02"]]]]);

// Setup event listener for GotAToken on OpenZeppelin TokenReceiver
let oz_gotAToken = lock();
oz_erc721tr.on("GotAToken", (...args) => {
  console.log(`OpenZeppelin GotAToken ${args}`);
  oz_gotAToken.unlock(args);
});

// Setup event listener for GotAToken on Reach TokenReceiver
let rch_gotAToken = lock();
rch_erc721tr.on("GotAToken", (...args) => {
  console.log(`Reach GotAToken ${args}`);
  rch_gotAToken.unlock(args);
});

// ===== Demonstration of ERC721TokenReceiver functionality =====
let operator, from, tokenId, data;
void(operator);
void(from);

console.log("OpenZeppelin ERC721 to OpenZeppelin ERC721TokenReceiver");
await oz_erc721.mint(oz_erc721tr.address, 0x1234, "0x1234");
[operator, from, tokenId, data] = await oz_gotAToken.wait;
oz_gotAToken.reset();
stdlib.assert(tokenId.eq(0x1234));
stdlib.assert(data === "0x1234");

console.log("OpenZeppelin ERC721 to Reach ERC721TokenReceiver");
await oz_erc721.mint(rch_erc721tr.address, 0x5678, "0x5678");
[operator, from, tokenId, data] = await rch_gotAToken.wait;
rch_gotAToken.reset();
stdlib.assert(tokenId.eq(0x5678));
stdlib.assert(data === "0x5678");

process.exit(0);
