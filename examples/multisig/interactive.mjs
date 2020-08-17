import * as MULTISIG from './build/index.main.mjs';
import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import { ask, yesno, runPart
       } from '@reach-sh/stdlib/command_line_helpers.mjs';

// Set up dependencies to run locally:
// (cd ../../js && npm link)
// npm install
// npm link "@reach-sh/stdlib"

const {toWeiBigNumber, hasRandom} = stdlib;
const name = process.argv[2];


// interact methods

const allowance = async () => {
  const amt = await ask(
    `How much would you like to deposit in ETH? (default: 50) > `,
    (x) => toWeiBigNumber(x || '50', 'ether')
  );

  console.log(`Parent deposits ${stdlib.fromWei(amt)} ETH`);
  return amt;
};

const approve = async (howMuch, balance) => {
  const ans = await ask(
    `Child requests ${stdlib.fromWei(howMuch)} ETH out of ${stdlib.fromWei(balance)} ETH.
  Do you approve? [y/n] (default: y) > `,
    (x) => yesno(x || 'y')
  );
  console.log(`Parent answers ${ans} to request for ${stdlib.fromWei(howMuch)}`);
  return ans;
};

const request = async (balance) => {
  const amt = await ask(
    `How much of ${stdlib.fromWei(balance)} ETH do you request in ETH? (default: 10) > `,
    (x) => stdlib.toWeiBigNumber(x || '10', 'ether')
  );
  console.log(`Child requests ${stdlib.fromWei(amt)} ETH out of ${stdlib.fromWei(balance)} ETH`);
  return amt;
};

const interact = {
  ...hasRandom,
  // Parent
  allowance,
  approve,
  // Child
  request,
};


runPart({
  PROG: MULTISIG,
  allPartNames: ['Parent', 'Child'],
  deployerPartName: 'Parent',
  thisPartName: name,
  greetBegin: `Let's start the multisig program.`,
  interact,
  stdlib,
});
