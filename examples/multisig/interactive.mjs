// import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as stdlib from '../../js/ETH.mjs';
import * as MULTISIG from './build/index.main.mjs';
import {ask, yesno, noFurtherQuestions} from './prompt.mjs';

// TODO: less copy/paste between examples; use a shared lib.

const {toWeiBN, newTestAccount, hasRandom} = stdlib;

const name = process.argv[2];

const promptCreateTestAccount = async () => {
  const start_amt = await ask(
    `How much ETH would you like in your test account? (default: 100) > `,
    (x) => toWeiBN(x || '100', 'ether')
  );

  console.log(`Creating account...`);
  const account = await newTestAccount(start_amt);
  console.log(`...account created.`);

  return account;
};

const displayCtc = (ctc) => {
  console.log(`{"address": "${ctc.address}", "creation_block": ${ctc.creation_block}}`);
};

const askCtc = async () => {
  return await ask(
    `Paste Alice's contract info here > `,
    (objStr) => {
      const obj = JSON.parse(objStr);
      if (!obj.address) { throw Error(`Missing address`); }
      else if (!obj.creation_block) { throw Error(`Missing creation_block`); }
      else { return obj; }
    }
  );
};

// interact methods

const allowance = async () => {
  const amt = await ask(
    `How much would you like to deposit in ETH? (default: 50) > `,
    (x) => toWeiBN(x || '50', 'ether')
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
    (x) => stdlib.toWeiBN(x || '10', 'ether')
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


const runParent = async () => {
  console.log(`Hello, Parent. Let's start the multisig program.`);
  console.log(`First, we'll connect to the test net.`);
  const account = await promptCreateTestAccount();

  console.log(`Next, we'll deploy the contract.`);
  const ctc = await account.deploy(MULTISIG);

  console.log(`Show Child the deployed contract info:`);
  displayCtc(ctc);

  console.log(`Alright, let's start!`);
  await MULTISIG.Parent(stdlib, ctc, interact);
  noFurtherQuestions();
};


const runChild = async () => {
  console.log(`Hello, Child. Let's start a game of Rock, Paper, Scissors.`);
  console.log(`First, we'll connect to the test net.`);
  const account = await promptCreateTestAccount();

  console.log(`Now, ask Parent about the contract info.`);
  console.log(`Next, we'll attach to Parent's contract.`);
  const ctcParent = await askCtc();
  const ctc = await account.attach(MULTISIG, ctcParent);

  console.log(`Alright, let's start!`);
  await MULTISIG.Child(stdlib, ctc, interact);
  noFurtherQuestions();
};

if (name === 'Parent') {
  runParent();
} else if (name === 'Child') {
  runChild();
} else {
  throw Error(`Expected Parent or Child, got ${name}`);
}
