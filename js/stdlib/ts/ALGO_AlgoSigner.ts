/*
import * as msgpack from '@msgpack/msgpack';
// DEBUG: uncomment this for debugging in browser
// @ts-ignore
// import algosdk__src__transaction from 'algosdk/src/transaction';

type AlgoSigner = {
  sign: (txn: Transaction) => Promise<{blob: string, txID: string}>,
  accounts: (args: {ledger: string}) => Promise<Array<{address: string}>>,
};

async function signTxn(networkAccount: NetworkAccount, txnOrig: Transaction): Promise<STX> {
  const {sk, AlgoSigner} = networkAccount;
  if (sk && !AlgoSigner) {
    const tx = txnOrig.signTxn(sk);
    const ret = {
      tx,
      txID: txnOrig.txID().toString(),
      lastRound: txnOrig.lastRound,
    };
    debug('signed sk_ret');
    debug({txID: ret.txID});
    debug(msgpack.decode(ret.tx));
    return ret;
  } else if (AlgoSigner) {
    // TODO: clean up txn before signing
    const txn = clean_for_AlgoSigner(txnOrig);

    // Note: don't delete the following,
    // it is extremely useful for debugging when stuff changes wrt AlgoSigner/algosdk clashes

    // if (sk) {
    //   const re_tx = txnOrig.signTxn ? txnOrig : new algosdk__src__transaction.Transaction(txnOrig);
    //   re_tx.group = txnOrig.group;

    //   const sk_tx = re_tx.signTxn(sk);
    //   const sk_ret = {
    //     tx: sk_tx,
    //     txID: re_tx.txID().toString(),
    //     lastRound: txnOrig.lastRound,
    //   };
    //   console.log('signed sk_ret');
    //   console.log({txID: sk_ret.txID});
    //   console.log(msgpack.decode(sk_ret.tx));
    // }

    debug('AlgoSigner.sign ...');
    const stx_obj = await AlgoSigner.sign(txn);
    debug('...signed');
    debug({stx_obj});
    const ret = {
      tx: Buffer.from(stx_obj.blob, 'base64'),
      txID: stx_obj.txID,
      lastRound: txnOrig.lastRound,
    };

    debug('signed AlgoSigner');
    debug({txID: ret.txID});
    debug(msgpack.decode(ret.tx));
    return ret;
  } else {
    throw Error(`networkAccount has neither sk nor AlgoSigner: ${JSON.stringify(networkAccount)}`);
  }
}

const [getAlgoSigner, setAlgoSigner] = replaceableThunk<Promise<AlgoSigner>>(async () => {
  if (window.AlgoSigner) {
    const AlgoSigner = window.AlgoSigner;
    await AlgoSigner.connect();
    return AlgoSigner;
  } else {
    // TODO: wait for a few seconds and try again before giving up
    throw Error(`Can't find AlgoSigner. Please refresh the page and try again.`);
  }
});
export { setAlgoSigner };

// XXX The getDefaultAccount pattern doesn't really work w/ AlgoSigner
// AlgoSigner does not expose a "currently-selected account"
export async function getDefaultAccount(): Promise<Account> {
  if (!window.prompt) {
    throw Error(`Cannot prompt the user for default account with window.prompt`);
  }
  const signStrategy = getSignStrategy();
  if (signStrategy === 'mnemonic') {
    const mnemonic = window.prompt(`Please paste the mnemonic for your account, or cancel to generate a new one`);
    if (mnemonic) {
      debug(`Creating account from user-provided mnemonic`);
      return await newAccountFromMnemonic(mnemonic);
    } else {
      debug(`No mnemonic provided. Randomly generating a new account secret instead.`);
      return await createAccount();
    }
  } else if (signStrategy === 'AlgoSigner') {
    const AlgoSigner = await getAlgoSigner();
    const ledger = getLedgerFromAlgoSigner(AlgoSigner);
    if (ledger === undefined) throw Error(`Ledger is undefined; this is required by AlgoSigner`);
    const addr = window.prompt(`Please paste your account's address. (This account must be listed in AlgoSigner.)`);
    if (!addr) { throw Error(`No address provided`); }
    return await newAccountFromAlgoSigner(addr, AlgoSigner, ledger);
  } else if (signStrategy === 'MyAlgo') {
    throw Error(`MyAlgo wallet support is not yet implemented`);
  } else {
    throw Error(`signStrategy '${signStrategy}' not recognized. Valid options are 'mnemonic', 'AlgoSigner', and 'MyAlgo'.`);
  }
}

function envDefaultALGOLedger(ledger: string|undefined, defaultLedger: string|undefined, server: string, port: string): string|undefined {
  // Some simple guessing
  // port is not currently used for this guessing, but could be in the future
  void(port);
  return ledger !== undefined ? ledger
    : serverLooksLikeRandlabs(server) ? guessRandlabsLedger(ledger)
    : defaultLedger;
}
function serverLooksLikeRandlabs(server: string): boolean {
  return server.toLowerCase().includes('algoexplorerapi.io');
}

function guessRandlabsLedger(server?: string): string|undefined {
  if (server === undefined) return undefined;
  server = server.toLowerCase();
  if (server.startsWith('https://algoexplorerapi.io')) {
    return 'MainNet';
  } else if (server.startsWith('https://testnet.algoexplorerapi.io')) {
    return 'TestNet';
  } else if (server.startsWith('https://betanet.algoexplorerapi.io')) {
    return 'BetaNet';
  }
  return undefined;
}

export const [getLedger, setLedger] = replaceableThunk<string|undefined>(() => DEFAULT_ALGO_LEDGER);
function getLedgerFromAlgoSigner(AlgoSigner: AlgoSigner) {
  // XXX: get AlgoSigner to tell us what Ledger is "currently selected"
  // since that ability doesn't actually exist, we operate based off of setLedger()
  void(AlgoSigner);
  return getLedger();
}

// XXX must be run before AlgoSigner signs
function regroup(thisAcc: NetworkAccount, txns: Array<Transaction>): Array<Transaction> {
  // Sorry this is so dumb.
  // Basically, if these go thru AlgoSigner,
  // it will mangle them,
  //  so we need to recalculate the group hash.
  if (thisAcc.AlgoSigner) {
    const roundtrip_txns = txns
      .map(x => clean_for_AlgoSigner(x))
      .map(x => unclean_for_AlgoSigner(x));

    // console.log(`deployP: group`);
    // console.log(txns[0].group);
    // console.log(Buffer.from(txns[0].group, 'base64').toString('base64'));
    // console.log({...txns[0]});

    algosdk.assignGroupID(roundtrip_txns);

    // console.log(`deploy: roundtrip group`);
    // console.log(Buffer.from(roundtrip_txns[0].group, 'base64').toString('base64'));

    const group = roundtrip_txns[0].group;
    // The same thing, but more paranoid:
    // const group = Buffer.from(roundtrip_txns[0].group, 'base64').toString('base64');
    for (const txn of txns) {
      txn.group = group;
    }
    // console.log({...txns[0]});

    return roundtrip_txns;
  } else {
    return txns;
  }
}

// A copy/paste of some logic from AlgoSigner
// packages/extension/src/background/messaging/task.ts
function unclean_for_AlgoSigner(txnOrig: any) {
  const txn = {...txnOrig};
  Object.keys({...txnOrig}).forEach(key => {
    if (txn[key] === undefined || txn[key] === null){
        delete txn[key];
    }
  });

  // Modify base64 encoded fields
  if ('note' in txn && txn.note !== undefined) {
      txn.note = new Uint8Array(Buffer.from(txn.note));
  }
  // Application transactions only
  if (txn && txn.type === 'appl'){
    if ('appApprovalProgram' in txn){
      txn.appApprovalProgram = base64ToUI8A(txn.appApprovalProgram);
    }
    if ('appClearProgram' in txn){
      txn.appClearProgram = base64ToUI8A(txn.appClearProgram);
    }
    if ('appArgs' in txn){
      var tempArgs: Array<Uint8Array> = [];
      txn.appArgs.forEach((element: string) => {
          tempArgs.push(base64ToUI8A(element));
      });
      txn.appArgs = tempArgs;
    }
  }

  // Note: this part is not copy/pasted from AlgoSigner,
  // and isn't even strictly necessary,
  // but it is nice for getting the same signatures from algosdk & AlgoSigner
  if ('group' in txn) {
    txn.group = base64ToUI8A(txn.group);
  }
  return txn;
}

const clean_for_AlgoSigner = (txnOrig: any) => {
  // Make a copy with just the properties, because reasons
  const txn = {...txnOrig};

  // AlgoSigner does weird things with fees if you don't specify flatFee
  txn.flatFee = true;

  // "Creation of PaymentTx has extra or invalid fields: name,tag,appArgs."
  delete txn.name;
  delete txn.tag;

  // uncaught (in promise) lease must be a Uint8Array.
  // it is... but how about we just delete it instead
  // This is presumed safe when lease is empty
  if (txn.lease instanceof Uint8Array && txn.lease.length === 0) {
    delete txn.lease;
  } else {
    console.log(txn.lease);
    throw Error(`Impossible: non-empty lease`);
  }

  // Creation of ApplTx has extra or invalid fields: nonParticipation
  if (!txn.nonParticipation) {
    delete txn.nonParticipation;
  } else {
    throw Error(`Impossible: expected falsy nonParticipation, got: ${txn.nonParticipation}`);
  }

  // "Creation of ApplTx has extra or invalid fields: name,tag."
  if (txn.type !== 'appl') {
    delete txn.appArgs;
  } else {
    if (txn.appArgs) {
      if (txn.appArgs.length === 0) {
        txn.appArgs = [];
      } else {
        txn.appArgs = txn.appArgs.map((arg: Uint8Array) => uint8ArrayToStr(arg, 'base64'));
      }
    }
  }

  // Validation failed for transaction because of invalid properties [from,to]
  // closeRemainderTo can cause an error w/ js-algorand-sdk addr parsing
  for (const field of ['from', 'to', 'closeRemainderTo']) {
    if (txn[field] && txn[field].publicKey) {
      txn[field] = algosdk.encodeAddress(txn[field].publicKey);
    }
  }

  // Weirdly, AlgoSigner *requires* the note to be a string
  // note is the only field that needs to be utf8-encoded, so far...
  for (const field of ['note']) {
    if (txn[field] && typeof txn[field] !== 'string') {
      txn[field] = uint8ArrayToStr(txn[field], 'utf8');
    }
  }

  // Uncaught (in promise) First argument must be a string, Buffer, ArrayBuffer, Array, or array-like object.
  // No idea what it's talking about, but probably GenesisHash?
  // And some more uint8Array BS
  for (const field of ['genesisHash', 'appApprovalProgram', 'appClearProgram', 'group']) {
    if (txn[field] && typeof txn[field] !== 'string') {
      txn[field] = uint8ArrayToStr(txn[field], 'base64');
    }
  }

  return txn;
};

export const newAccountFromAlgoSigner = async (addr: string, AlgoSigner: AlgoSigner, ledger: string) => {
  if (!AlgoSigner) {
    throw Error(`AlgoSigner is falsy`);
  }
  const accts = await AlgoSigner.accounts({ledger});
  if (!Array.isArray(accts)) {
    throw Error(`AlgoSigner.accounts('${ledger}') is not an array: ${accts}`);
  }
  if (!accts.map(x => x.address).includes(addr)) {
    throw Error(`Address ${addr} not found in AlgoSigner accounts`);
  }
  let networkAccount = {addr, AlgoSigner};
  return await connectAccount(networkAccount);
};

function envDefaultALGOPort(port: string|undefined, defaultPort: string, server: string): string {
  // Some simple guessing
  return port !== undefined ? port
    : serverLooksLikeRandlabs(server) ? ''
    : defaultPort;
}

function envDefaultALGOToken(token: string|undefined, defaultToken: string, server: string, port: string): string {
  // Some simple guessing
  // port is not currently used for this guessing, but could be in the future
  void(port);
  return token !== undefined ? token
    : serverLooksLikeRandlabs(server) ? ''
    : defaultToken;
}

*/
export {};
