import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as MULTISIG from './build/index.main.mjs';

( async () => {
  const startingBalance = stdlib.toWeiBigNumber('100', 'ether');
  const smallest = stdlib.toWeiBigNumber('1', 'ether');

  const parent = await stdlib.newTestAccount(startingBalance);
  const parentCtc = await parent.deploy(MULTISIG);
  console.log(`Parent deploys the contract.`);
  const parentInteract = {
    allowance: () => {
      const amt = stdlib.toWeiBigNumber('50', 'ether');
      console.log(`Parent deposits ${stdlib.fromWei(amt)}`);
      return amt; },
    approve: (howMuch, balance) => {
      const ans = stdlib.le( balance, smallest ) || stdlib.lt( howMuch, stdlib.div( balance, stdlib.bigNumberify(2) ) );
      console.log(`Parent answers ${ans} to request for ${stdlib.fromWei(howMuch)}`);
      return ans; } };
  const parentP = MULTISIG.Parent(stdlib, parentCtc, parentInteract);

  const child = await stdlib.newTestAccount(startingBalance);
  const childCtc = await child.attach(MULTISIG, parentCtc);
  const UNITS = 8;
  const childInteract = {
    request: (balance) => {
      const amt = stdlib.le( balance, smallest ) ? balance : stdlib.mul( stdlib.bigNumberify( Math.floor(Math.random() * UNITS) ), stdlib.div( balance, stdlib.bigNumberify( UNITS ) ) );
      console.log(`Child asks for ${stdlib.fromWei(amt)} out of ${stdlib.fromWei(balance)}`);
      return amt; } };
  const childP = MULTISIG.Child( stdlib, childCtc, childInteract);

  const parentO = await parentP;
  const childO = await childP;

  void(parentO, childO);

  console.log(`\nMulti-sig complete\n`);
  process.exit(0); })();
