import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';
import ethers from 'ethers';
import * as fs from 'fs';
import launchToken from '@reach-sh/stdlib/launchToken.mjs';

(async () => {
  const stdlib = await stdlib_loader.loadStdlib();
  if ( stdlib.connector !== 'ETH' ) { return; }
  const assertEq = (expected, actual) => {
    const exps = JSON.stringify(expected);
    const acts = JSON.stringify(actual);
    console.log('assertEq', {expected, actual}, {exps, acts});
    stdlib.assert(exps === acts) };
  const getEvt = (id, obj) => {
    for ( const v of obj ) {
      if ( v.name === id && v.type === 'event' ) {
        return v.inputs;
      }
    }
    return [];
  };

  console.log(`Alice remote: make factory`);
  const compiled = JSON.parse(await fs.readFileSync('./build/index.sol.json'));
  console.log(`Alice read compiled file: ${JSON.stringify(compiled)}`);
  const remoteCtc = compiled["contracts"]["index.sol:WeirdContract"];
  const remoteABI = remoteCtc["abi"];
  const remoteBytecode = remoteCtc["bin"];
  const which = '_reach_e1';
  assertEq(getEvt(which, JSON.parse(backend._Connectors.ETH.ABI)), getEvt(which, remoteABI));

  const startingBalance = stdlib.parseCurrency(100);
  const accAlice = await stdlib.newTestAccount(startingBalance);

  const myGasLimit = 5000000;
  accAlice.setGasLimit(myGasLimit);

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

  const amt = stdlib.parseCurrency(0.1);

  await Promise.all([
    backend.Alice(ctcAlice, {
      ...stdlib.hasConsoleLogger,
      get: (() => ({ ctcInfo: remoteAddr, x: 5 })),
    }),
  ]);
})();
