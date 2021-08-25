import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';
import real_ethers from 'ethers';
import * as cfxers from '@reach-sh/stdlib/cfxers.mjs';
import * as fs from 'fs';


(async () => {
  const stdlib = await stdlib_loader.loadStdlib();
  const ethers = stdlib.connector === 'CFX' ? cfxers : real_ethers;
  const startingBalance = stdlib.parseCurrency(10);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);
  const myGasLimit = 8000000;
  accAlice.setGasLimit(myGasLimit);
  accBob.setGasLimit(myGasLimit);
  
  const compiled = JSON.parse(await fs.readFileSync('./build/index.sol.json'));
  console.log(`Alice read compiled file: ${JSON.stringify(compiled)}`);
  const remoteCtc = compiled["contracts"]["index.sol:LogAttack1"];
  const remoteABI = remoteCtc["abi"];
  const remoteBytecode = remoteCtc["bin"];
  const factory = new ethers.ContractFactory(remoteABI, remoteBytecode, accAlice.networkAccount);
  const contract = await factory.deploy({ gasLimit: myGasLimit });
  await contract.deployed();
  console.log(`SOLIDITY CONTRACT ADDR : ${contract.address}`);
  console.log(`Tx Hash: ${contract.deployTransaction.hash}`);
  const remoteAddr = contract.address;
  
  const ctcAlice = accAlice.deploy(backend);
  const ctcBob = accBob.attach(backend, ctcAlice.getInfo());
  const amt = stdlib.parseCurrency(1);

  await Promise.all([
    backend.Alice(ctcAlice, {
      ...stdlib.hasConsoleLogger,
      m2: (() => [ remoteAddr, amt ]),
    }),
    backend.Bob(ctcBob, {
      ...stdlib.hasConsoleLogger,
    })
  ]);
})();