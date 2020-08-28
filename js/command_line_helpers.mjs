import { ask, done } from './ask.mjs';
export * from './ask.mjs';

// ======== ETH tools ============

// There are three levels of abstraction available
// (pick one):
//
// Highest: do all the things
// runPart(opts);
//
// Next highest: define runners for each part
// const runA = mkPart({...opts, thisPart: 'A'});
// const runB = mkPart({...opts, thisPart: 'B'});
//
// Lower: use the common tools to define runners for each part
// const runA = async () => {
//   const account = promptCreateTestAccount();
//   const ctc = account.deploy(PROG);
//   console.log(shareableCtcStr(ctc));
//   return await PROG.A(stdlib, ctc, interact);
// };
// const runB = async () => {
//   const account = promptCreateTestAccount();
//   const ctcDeployer = askCtc(`paste ctc pls > `);
//   const ctc = account.attach(PROG, ctcDeployer);
//   return await PROG.B(stdlib, ctc, interact);
// };

// TODO abstract so it's not specific to ETH
export const promptCreateTestAccount = async (stdlib) => {
  const start_amt = await ask(
    `How much ETH would you like in your test account? (default: 100) > `,
    (x) => stdlib.toWeiBigNumber(x || '100', 'ether')
  );

  console.log(`Creating account...`);
  const account = await stdlib.newTestAccount(start_amt);
  console.log(`...account created.`);

  return account;
};

export const shareableCtcStr = (ctc) => (
  `{"address": "${ctc.address}", "creation_block": ${ctc.creation_block}}`
);

export const askCtc = async (prompt) => {
  return await ask(
    prompt,
    (objStr) => {
      const obj = JSON.parse(objStr);
      if (!obj.address) { throw Error(`Missing address`); }
      else if (!obj.creation_block) { throw Error(`Missing creation_block`); }
      else { return obj; }
    }
  );
};

const getCtc_ = async (account, opts) => {
  const {PROG, allPartNames, thisPartName, deployerPartName} = opts;
  if (thisPartName === deployerPartName) {
    console.log(`Next, we'll deploy the contract.`);
    const ctc = await account.deploy(PROG);

    const otherParticipants =
          allPartNames.filter((x) => x !== deployerPartName);

    console.log(`Other participants are: ${otherParticipants}`);
    console.log(`Show the other participants your deployed contract info:`);
    console.log(shareableCtcStr(ctc));

    return ctc;
  } else {
    console.log(`Now, ask ${deployerPartName} for the contract info.`);
    console.log(`Next, we'll attach to ${deployerPartName}'s contract.`);
    const ctcDeployer = await askCtc(
      `Paste ${deployerPartName}'s contract info > `);
    return await account.attach(PROG, ctcDeployer);
  }
};

const mkPart_ = (opts) => async () => {
  const {PROG, thisPartName, greetBegin, interact, stdlib} = opts;

  console.log(`Hello, ${thisPartName}. ${greetBegin}`);
  console.log(`First, we'll connect to the test net.`);
  const account = await promptCreateTestAccount(stdlib);

  let ctc = await getCtc_(account, opts);

  console.log(`Alright, let's begin.`);
  await PROG[thisPartName](stdlib, ctc, interact);
  done();
};

const setDiff = (s1, s2) =>
      new Set([...s1].filter((x) => !s2.has(x)));

const ensureAllOpts = (opts) => {
  const optKeys = new Set(Object.keys(opts));
  const expectedKeys = new Set([
    'PROG',
    'allPartNames',
    'deployerPartName',
    'thisPartName',
    'greetBegin',
    'interact',
    'stdlib',
  ]);
  const missingKeys = setDiff(expectedKeys, optKeys);
  const extraKeys = setDiff(optKeys, expectedKeys);
  const failures = [];
  if (extraKeys.size > 0) {
    failures.add(`Extra keys: [${[...extraKeys]}]`);
    // tolerate extra keys if no missing keys.
  }
  if (missingKeys.size > 0) {
    failures.add(`Missing keys: [${[...missingKeys]}]`);
    throw Error(`[${[...failures]}]`);
  }
};

// Use this if you want to define part functions yourself.
export const mkPart = (opts) => {
  ensureAllOpts(opts);

  const {PROG, thisPartName} = opts;
  const partFn = PROG[thisPartName];
  if (!partFn) {
    throw Error(`Can't find ${thisPartName} in the contract,
  even though it's on the list.`);
  }

  if (typeof(partFn) !== 'function') {
    throw Error(`Found ${thisPartName} in the contract,
  but it's not a function...?`);
  }

  return mkPart_(opts);
};

// Use this if you don't want to define part functions yourself.
export const runPart = async (opts) => {
  ensureAllOpts(opts);

  const {PROG, allPartNames, thisPartName} = opts;
  if (!allPartNames.includes(thisPartName)) {
    if (PROG[thisPartName]) {
      throw Error(
        `Found part ${thisPartName} in the contract, but didn't expect it.
  Did you forget to add it to the list?`
      );
    } else {
      throw Error(`Got ${thisPartName}, but expected one of ${allPartNames}.`);
    }
  }

  const thisPart = mkPart(opts);
  await thisPart();
};
