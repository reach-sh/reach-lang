import { Reach } from '@reach-sh/stdlib';
import * as server from './build/index.server.mjs';
import * as fs from 'fs';

const r = new Reach();
if ( r.connector !== 'ETH' ) { process.exit(0); }
const { ethers } = r;

const acc = await r.newTestAccount(r.parseCurrency(100));
const gasLimit = 5_000_000;
acc.setGasLimit(gasLimit);

const ctcS = acc.contract(server);
const info = await r.withDisconnect(() =>
  ctcS.p.Admin({ ready: r.disconnect }));

const compiled = JSON.parse(await fs.readFileSync('./build/index.sol.json'));
const { abi, bin } = compiled["contracts"]["index.sol:WeirdContract"];
const factory = new ethers.ContractFactory(abi, bin, acc.networkAccount);
const contract = await factory.deploy({ gasLimit });
const deploy_r = await contract.deployTransaction.wait();
const addr = contract.address;

await ctcS.a.g(addr, true);

{
  let err;
  try {
    await ctcS.a.g(addr, false);
  } catch (e) {
    err = e;
  }
  if ( !err ) {
    throw new Error(`Expected error, but didn't get one for effect`);
  }
}

await ctcS.a.o();
