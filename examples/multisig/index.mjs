import * as stdlib from '@reach-sh/stdlib';
import * as MULTISIG from './build/multisig.mjs';

( async () => {
  const startingBalance = stdlib.toWeiBN('100', 'ether');
  const smallest = stdlib.toWeiBN('1', 'ether');

  const parent = await stdlib.newTestAccount(startingBalance);
  const parentCtc = await parent.deploy(MULTISIG);
  console.log(`Parent deploys the contract.`);
  const parentInteract = {
    allowance: () => {
      const amt = stdlib.toWeiBN('50', 'ether');
      console.log(`Parent deposits ${stdlib.fromWei(amt)}`);
      return amt; },
    approve: (howMuch, balance) => {
      const ans = stdlib.le( balance, smallest ) || stdlib.lt( howMuch, stdlib.div( balance, stdlib.toBN(2) ) );
      console.log(`Parent answers ${ans} to request for ${stdlib.fromWei(howMuch)}`);
      return ans; } };
  const parentP = MULTISIG.Parent(stdlib, parentCtc, parentInteract);

  const child = await stdlib.newTestAccount(startingBalance);
  const childCtc = await child.attach(MULTISIG, parentCtc.address, parentCtc.creation_block);
  const UNITS = 8;
  const childInteract = {
    request: (balance) => {
      const amt = stdlib.le( balance, smallest ) ? balance : stdlib.mul( stdlib.toBN( Math.floor(Math.random() * UNITS) ), stdlib.div( balance, stdlib.toBN( UNITS ) ) );
      console.log(`Child asks for ${stdlib.fromWei(amt)} out of ${stdlib.fromWei(balance)}`);
      return amt; } };
  const childP = MULTISIG.Child( stdlib, childCtc, childInteract);

  const parentO = await parentP;
  const childO = await childP;

  const showOutcome = ([ oks, nos ]) => `${oks} Yes, ${nos} No`;
  console.log(`Parent thinks outcome is ${showOutcome(parentO)}.`);
  console.log(`Child thinks outcome is ${showOutcome(childO)}.`);

  console.log(`\nMulti-sig complete\n`);
  process.exit(0); })();
