import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import * as fs from 'fs';

const stdlib = loadStdlib();

if ( stdlib.connector !== 'ETH' ) { process.exit(0); }
const { ethers } = stdlib;
const assertEq = (expected, actual) => {
  const exps = JSON.stringify(expected, null, 2);
  const acts = JSON.stringify(actual, null, 2);
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

const compiled = JSON.parse(await fs.readFileSync('./build/index.sol.json'));
console.log(`Alice read compiled file: ${JSON.stringify(compiled)}`);
const remoteCtc = compiled["contracts"]["index.sol:LogAttack1"];
const remoteABI = remoteCtc["abi"];
const remoteBytecode = remoteCtc["bin"];
const which = '_reach_e1';
assertEq(getEvt(which, JSON.parse(backend._Connectors.ETH.ABI)), getEvt(which, remoteABI));

const startingBalance = stdlib.parseCurrency(100);
const accAlice = await stdlib.newTestAccount(startingBalance);
const accBob = await stdlib.newTestAccount(startingBalance);

const myGasLimit = 8000000;
accAlice.setGasLimit(myGasLimit);
accBob.setGasLimit(myGasLimit);

const factory = new ethers.ContractFactory(remoteABI, remoteBytecode, accAlice.networkAccount);
const contract = await factory.deploy({ gasLimit: myGasLimit });
await contract.deployed();
const remoteAddr = contract.address;

const ctcAlice = accAlice.contract(backend);
const ctcBob = accBob.contract(backend, ctcAlice.getInfo());
const amt = stdlib.parseCurrency(1);
const exp = [ remoteAddr, amt ];

await Promise.all([
  backend.Alice(ctcAlice, {
    get: (() => exp),
  }),
  backend.Bob(ctcBob, {
    check: (...act) =>
      assertEq(exp, act)
  })
]);
