import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import * as guessBackend from './build/guess.main.mjs';
import real_ethers from 'ethers';
import * as cfxers from '@reach-sh/stdlib/cfxers.mjs';
import * as fs from 'fs';

const stdlib = loadStdlib(process.env);

(async () => {
  if ( stdlib.connector === 'ALGO' ) { return; }
  const ethers = stdlib.connector === 'CFX' ? cfxers : real_ethers;

  const startingBalance = stdlib.parseCurrency(100);
  const [ accAlice, accBob, accChi ] =
    await stdlib.newTestAccounts(3, startingBalance);

  const gasLimit = 5000000;
  accAlice.setDebugLabel('Alice').setGasLimit(gasLimit);
  accBob.setDebugLabel('Bob').setGasLimit(gasLimit);
  const compiled = JSON.parse(await fs.readFileSync('./build/index.sol.json'));
  const remoteCtc = compiled["contracts"]["index.sol:MyContract"];
  const remoteABI = remoteCtc["abi"];
  const remoteBytecode = remoteCtc["bin"];
  const factory = new ethers.ContractFactory(remoteABI, remoteBytecode, accBob.networkAccount);
  const contract = await factory.deploy({ gasLimit });
  await contract.deployTransaction.wait();
  const remoteAddr = contract.address;
  console.log(`Bob deployed: ${remoteAddr}`);

  const ctcAlice = accAlice.contract(backend);

  const callCtc = async () => {
    const ctcAddr = await ctcAlice.getContractAddress();
    const rand = Math.floor(Math.random() * 4) + 1;
    const x = await contract.callApi(ctcAddr, rand);
    await x.wait();
  }

  const ctcChi = accChi.setDebugLabel('Chi').contract(guessBackend);

  const v = 5;

  await Promise.all([
    guessBackend.A(ctcChi, {
      x: () => {
        console.log(`Returning guess value: ${v}`);
        return v;
      }
    }),
    backend.Alice(ctcAlice, {
      log: (...xs) => console.log(...xs),
      initCall: () => callCtc(),
      ctcInfo: async () => { return await ctcChi.getInfo() },
      getGuess: () => v,
    }),
  ]);
})();
