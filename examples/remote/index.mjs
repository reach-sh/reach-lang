import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';
import ethers from 'ethers';
import * as fs from 'fs';

(async () => {
  const stdlib = await stdlib_loader.loadStdlib();

  const startingBalance = stdlib.parseCurrency(1);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const myGasLimit = 5000000;
  accAlice.setGasLimit(myGasLimit);
  accBob.setGasLimit(myGasLimit);

  console.log(`Alice remote: make factory`);
  const compiled = JSON.parse(await fs.readFileSync('./build/index.sol.json'));
  console.log(`Alice read compiled file: ${JSON.stringify(compiled)}`);
  const remoteCtc = compiled["contracts"]["index.sol:WeirdContract"];
  const remoteABI = remoteCtc["abi"];
  const remoteBytecode = remoteCtc["bin"];
  const factory = new ethers.ContractFactory(remoteABI, remoteBytecode, accAlice.networkAccount);
  console.log(`Alice remote: deploy`);
  const contract = await factory.deploy({ gasLimit: myGasLimit });
  console.log(`Alice remote: wait for deploy: ${contract.deployTransaction.hash}`);
  const deploy_r = await contract.deployTransaction.wait();
  console.log(`Alice remote: saw deploy: ${deploy_r.blockNumber}`);
  const remoteAddr = contract.address;
  console.log(`Alice remote: deployed: ${remoteAddr}`);

  console.log(`Alice will deploy the Reach DApp.`);
  const ctcAlice = accAlice.deploy(backend);
  console.log(`Bob attaches to the Reach DApp.`);
  const ctcBob = accBob.attach(backend, ctcAlice.getInfo());

  const amt = stdlib.parseCurrency(0.1);
  await Promise.all([
    backend.Alice(ctcAlice, {
      getAddr: (() => remoteAddr),
      getCT: (() => [ amt, remoteAddr ]),
    }),
    backend.Bob(ctcBob, {
      getX: (() => 4),
      see: ((...v) => console.log(`Bob sees ${JSON.stringify(v)}`)),
    }),
  ]);
})();
