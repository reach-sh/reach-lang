import * as fs from 'fs';
import { loadStdlib, ask } from "@reach-sh/stdlib";
const stdlib = loadStdlib(process.env);
const ethers = stdlib.ethers;

if (stdlib.connector !== "ETH") {
  console.log("This test only applies to ETH");
  process.exit(0);
}

//const acc = await stdlib.newAccountFromMnemonic(await ask.ask("input secret:", (x => x)));
const acc = await stdlib.newTestAccount(stdlib.parseCurrency(1))

const gasLimit = { gasLimit: 5_000_000 };
const zeroAddr = "0x" + "0".repeat(40);
const waitTxn = async callPromise => await (await callPromise).wait();


const deploy = async (abi, bin, args = []) => {
  const factory = new ethers.ContractFactory(abi, bin, acc.networkAccount);
  const contract = await factory.deploy(...args, gasLimit);
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
const rchConnect = async (rchModulePath, ctcAddr, signerAddr) => {
  const mod = await import(rchModulePath);
  const ctc = mod._Connectors.ETH;
  return new ethers.Contract(ctcAddr, ctc.ABI, await acc.getAddress())
}


const reach_erc721_constructor_args = [
  [
    // time
    0,
    [
      // v3236, string, name
      "test",
      // v3237, string, symbol
      "TST",
      // v3238, string, tokenURI
      // This is an ipfs URI to some json files that have the ERC721 metadata schema that includes links to some bland screenshots I took
      // An ipfs:// link is too long with all other needed arguments.  We need fewer publish arguments, or to break them up.  For now I'm hard coding the totalSupply...
      "ipfs://QmXjZbPwhgSqY5X1mkAQv91K5FQmQzMkKhD2daLGJjv3V1/",
      //"ipfs://bafybeiels7a3cvxkrwm32azmnrtdw2fick74wg3dv3geromgn7vqd4frna/",
      // v3239, uint256, I think this is totalSupply
      2,
      // v3240, address payable, I think this is the zero address
      zeroAddr,
      // Empty BytesDyn
      [],
    ],
  ],
];
const reachERC721Deploy = async () => {
  return await rchDeploy("./build/index.main.mjs", reach_erc721_constructor_args);
}

const doDeploy = async () => {
  console.log("Deploying...")
  const [ctc, deployGasUsed] = await reachERC721Deploy()
  console.log("Deployed, ctc: ", ctc.address);
  console.log("Deployed, gas used: ", deployGasUsed.toNumber());
  return ctc;
}

const g = async (ctc, acc, f, ...args) => {
  const cWithAddr = ctc.connect(acc.networkAccount);
  const fn = cWithAddr[f];
  if(fn) {
    const txn = (await (await fn(...args, gasLimit)).wait())
    const gasNum = txn.gasUsed.toNumber();
    //console.log("txn: ", txn);
    return gasNum;
  } else {
    return "N/A";
  }
}

const doMint = async (ctc) => {
  console.log("Minting...")
  //ctc.on("Transfer", (...args) => console.log("Transfer args: ", args));
  const mint1Gas = await g(ctc, acc, "mint", await acc.getAddress(), 1);
  console.log("mint 1 gas:", mint1Gas);
  const mint2Gas = await g(ctc, acc, "mint", await acc.getAddress(), 2);
  console.log("mint 2 gas:", mint2Gas);
}

const doDeployAndMint = async () => {
  const ctc = await doDeploy();
  await doMint(ctc);
  return ctc;
}


const theCtc = await doDeployAndMint();
//console.log(await theCtc.ownerOf(1))
//console.log(await theCtc.ownerOf(2))


process.exit(0);
