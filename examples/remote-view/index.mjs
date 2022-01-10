import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import real_ethers from 'ethers';
import * as cfxers from '@reach-sh/stdlib/cfxers.mjs';
import * as fs from 'fs';

const stdlib = loadStdlib(process.env);

(async () => {
  if ( stdlib.connector === 'ALGO' ) { return; }
  const ethers = stdlib.connector === 'CFX' ? cfxers : real_ethers;

  const startingBalance = stdlib.parseCurrency(100);
  const [ accAlice, accBob ] =
    await stdlib.newTestAccounts(2, startingBalance);


  const gasLimit = 5000000;
  accAlice.setGasLimit(gasLimit);
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

  let count = 1;

  await Promise.all([
    backend.Alice(ctcAlice, {
      log: (x) => console.log(`Saw `, x.toNumber()),
      keepGoing: async () => {
        const kg = count < 5;
        const ctcAddr = await ctcAlice.getContractAddress();
        const xt = await contract["callView"](ctcAddr);
        const xr = await xt.wait();
        const ls = xr.logs;
        const l = ls[0];
        console.log(`keepGoing r`, { xt, xr, ls });
        stdlib.assert(l, 'log');
        const { name, args } = contract.interface.parseLog(l);
        console.log(`keepGoing`, name, args);
        const x = args[0];
        console.log(`Compare: `, x.toNumber(), count);
        stdlib.assert(x == count, "x == count");
        count = count + 1;
        return [kg, count];
      }
    }),
  ]);
})();
