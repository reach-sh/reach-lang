import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';
import real_ethers from 'ethers';
import * as cfxers from '@reach-sh/stdlib/cfxers.mjs';
import * as fs from 'fs';
import launchToken from '@reach-sh/stdlib/launchToken.mjs';

(async () => {
  const stdlib = await stdlib_loader.loadStdlib();
  if ( stdlib.connector === 'ALGO' ) { return; }
  const ethers = stdlib.connector === 'CFX' ? cfxers : real_ethers;

  const startingBalance = stdlib.parseCurrency(100);
  const [ accAlice, accBob, accCreator ] = await stdlib.newTestAccounts(3, startingBalance);

  const myGasLimit = 5000000;
  accAlice.setDebugLabel('Alice').setGasLimit(myGasLimit);
  accBob.setDebugLabel('Bob').setGasLimit(myGasLimit);
  accCreator.setDebugLabel('Creator').setGasLimit(myGasLimit);

  const gil = await launchToken(stdlib, accCreator, "gil", "GIL");
  await gil.mint(accAlice, startingBalance);
  await gil.mint(accAlice, startingBalance);

  const zorkmid = await launchToken(stdlib, accCreator, "zorkmid", "ZMD");
  await zorkmid.mint(accAlice, startingBalance);
  await zorkmid.mint(accAlice, startingBalance);

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
      getCTX: (() => remoteAddr),
      getCTY: (() => [ amt, remoteAddr ]),
      getGIL: (() => gil.id),
      getZMD: (() => zorkmid.id),
    }),
    backend.Bob(ctcBob, {
      getX: (() => 4),
      see: ((...v) => console.log(`Bob sees ${JSON.stringify(v)}`)),
    }),
  ]);
})();
