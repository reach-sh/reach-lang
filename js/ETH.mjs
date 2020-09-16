import Timeout from 'await-timeout';
import ethers from 'ethers';
import * as http from 'http';
import * as url from 'url';
import * as waitPort from 'wait-port';

import { getConnectorMode } from './loader.mjs';
import {
  add,
  assert,
  bigNumberify,
  debug,
  ge,
  getDEBUG,
  isBigNumber,
  keccak256,
  lt,
} from './shared.mjs';
export * from './shared.mjs';

// Note: if you want your programs to exit fail
// on unhandled promise rejection, use:
// node --unhandled-rejections=strict

// XXX: when using ganache: nvm use 12
// ganache-core doesn't work with npm version 14 yet
// https://github.com/trufflesuite/ganache-cli/issues/732#issuecomment-623782405

// networkAccount[ETH] = {
//   // Required for receivers
//   address: string
//
//   // Required for senders
//   sendTransaction: function
//
//   // Must be an ethers.Wallet to deploy or attach to a contract.
// }
//
// ctc[ETH] = {
//   address: string
//   creation_block: int
//
//   // More fields indicating initialization status
//   args: array<array<string>>. See comment on reallyDeploy
//   value: UInt256 or number or string or something.
//
//   // internal fields
//   // * not required to call acc.attach(bin, ctc)
//   // * required by backend
//   sendrecv: function
//   recv: function
// }

const connectorMode = getConnectorMode();

// Certain functions either behave differently,
// or are only available on an "isolated" network.
const isIsolatedNetwork =
  connectorMode.startsWith('ETH-test-dockerized') ||
  connectorMode.startsWith('ETH-test-embedded');

// Unique helpers

export const toWei = (amt, unit) =>
  ethers.utils.parseUnits(amt, unit || 'ether');
export const fromWei = (amt, unit) =>
  ethers.utils.formatUnits(amt, unit || 'ether');
export const toWeiBigNumber = (a, b) => bigNumberify(toWei(a, b));

// end Unique helpers

// private helpers

const flaky = async (f) => {
  const max_tries = 3;
  const sleep_between_tries = 1000; // ms
  let failed_attempts = 0;
  while (true) {
    try {
      // await doHealthcheck();
      return await f();
    } catch (e) {
      failed_attempts++;
      if (failed_attempts >= max_tries) {
        throw e;
      } else {
        debug(`FAILED ATTEMPT # ${failed_attempts}...`);
        await Timeout.set(sleep_between_tries);
        debug('trying again...');
      }
    }
  }
};
void(flaky); // XXX

// end private helpers

// Common interface exports

const networkDesc = connectorMode == 'ETH-test-embedded-ganache' ? {
  type: 'embedded-ganache',
} : connectorMode == 'ETH-test-dockerized-geth' ? {
  type: 'uri',
  uri: process.env.ETH_NODE_URI || 'http://localhost:8545',
  network: process.env.ETH_NODE_NETWORK || 'unspecified',
} : {
  type: 'skip',
};

const portP = (async () => {
  if (networkDesc.type != 'uri') { return; }
  const { hostname, port, path } = url.parse(networkDesc.uri);
  const params = {
    protocol: 'http' // XXX no apparent need to support https
      ,
    host: hostname,
    port: parseInt(port, 10),
    path,
    'output': 'silent',
    'timeout': 1000 * 60 * 1,
  };
  await waitPort.default(params);
})();

// XXX: doesn't even retry, just returns the first attempt
const doHealthcheck = async () => {
  if (networkDesc.type != 'uri') { return; }
  await new Promise((resolve, reject) => {
    const { hostname, port } = url.parse(networkDesc.uri);
    const data = JSON.stringify({
      jsonrpc: '2.0',
      method: 'web3_clientVersion',
      params: [],
      id: 67,
    });
    debug('Sending health check request...');
    const opts = {
      hostname,
      port,
      path: '/',
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Content-Length': data.length,
      },
    };
    const req = http.request(opts, (res) => {
      debug(`statusCode: ${res.statusCode}`);
      res.on('data', (d) => {
        debug('rpc health check succeeded');
        if (getDEBUG()) {
          process.stdout.write(d);
        }
        resolve({ res, d });
      });
    });
    req.on('error', (e) => {
      console.log('rpc health check failed');
      console.log(e);
      reject(e);
    });
    req.write(data);
    debug('attached all the handlers...');
    req.end();
    debug('req.end...');
  });
};

const devnetP = (async () => {
  await portP;
  debug('Got portP, waiting for health');
  return await doHealthcheck();
})();

// async () => provider
const getProvider = (() => {
  const etherspP = (async () => {
    if (networkDesc.type == 'uri') {
      await devnetP;
      const provider = new ethers.providers.JsonRpcProvider(networkDesc.uri);
      provider.pollingInterval = 500; // ms
      return provider;
    } else if (networkDesc.type == 'embedded-ganache') {
      const { default: ganache } = await import('ganache-core');
      const default_balance_ether = '999999999';
      const ganachep = ganache.provider({ default_balance_ether });
      return new ethers.providers.Web3Provider(ganachep);
    } else {
      // This lib was imported, but not for its net connection.
      return null;
    }
  })();

  return async () => {
    const provider = await etherspP;
    if (provider === null) {
      throw Error(`Using stdlib/ETH is incompatible with REACH_CONNECTOR_MODE=${connectorMode}`);
    } else {
      return provider;
    }
  };
})();


// XXX expose setProvider

const ethersBlockOnceP = async () => {
  const provider = await getProvider();
  return new Promise((resolve) => provider.once('block', (n) => resolve(n)));
};

export const balanceOf = async acc => {
  const { networkAccount } = acc;
  if (!networkAccount) throw Error(`acc.networkAccount missing. Got: ${acc}`);

  if (networkAccount.getBalance) {
    return bigNumberify(await acc.networkAccount.getBalance());
  } else if (networkAccount.address) {
    const provider = await getProvider();
    return bigNumberify(await provider.getBalance(networkAccount.address));
  } else throw Error(`acc.networkAccount.address missing. Got: ${networkAccount}`);
};

// XXX dead code?
// `t` is a type name in string form; `v` is the value to cast
// const encode = (t, v) =>
//   ethers.utils.defaultAbiCoder.encode([t], [v]);

// Arg order follows "src before dst" convention
// From and to may be:
// * an acc
// * a networkAccount
// From may also be:
// * getSigner()
export const transfer = async (from, to, value) => {
  if (from.networkAccount) return await transfer(from.networkAccount, to, value);
  if (to.networkAccount) return await transfer(from, to.networkAccount, value);
  if (!to.address) throw Error(`Expected to.address: ${to}`);
  if (!from.sendTransaction) throw Error(`Expected from.sendTransaction: ${from}`);
  if (!isBigNumber(value)) throw Error(`Expected a BigNumber: ${value}`);

  const txn = { to: to.address, value };
  debug(`from.sendTransaction(${JSON.stringify(txn)})`);
  return await from.sendTransaction(txn);
};

// Helpers for sendrecv and recv

const rejectInvalidReceiptFor = async (txHash, r) =>
  new Promise((resolve, reject) =>
    !r ? reject(`No receipt for txHash: ${txHash}`) :
    r.transactionHash !== txHash ? reject(`Bad txHash; ${txHash} !== ${r.transactionHash}`) :
    !r.status ? reject(`Transaction: ${txHash} was reverted by EVM\n${r}`) :
    resolve(r));

const fetchAndRejectInvalidReceiptFor = async txHash => {
  const provider = await getProvider();
  const r = await provider.getTransactionReceipt(txHash);
  return await rejectInvalidReceiptFor(txHash, r);
};

export const connectAccount = async networkAccount => {
  // XXX networkAccount MUST be a wallet to deploy/attach
  const provider = await getProvider();
  const { address } = networkAccount;
  const shad = address.substring(2, 6);

  const extractInfo = async (ctcOrInfo) => {
    const { getInfo, address, creation_block, args, value, creator } = ctcOrInfo;
    if (getInfo) {
      return extractInfo(await getInfo());
    } else if (address && creation_block && args && value != undefined && creator) {
      return { address, creation_block, args, value, creator };
    } else {
      throw Error(`Expected contract information, got something else: ${JSON.stringify(ctcOrInfo)}`);
    }
  };

  const attach = async (bin, parentCtc) => {
    const ABI = JSON.parse(bin._Connectors.ETH.ABI);

    let info = null;
    let _waitForInfo = null;
    if (parentCtc.reallyDeploy) {
      _waitForInfo = async (internal, args, value) => {
        if (!internal) {
          return false;
        }
        if (args == null || value == null) {
          throw Error(`Out of order sendrecv`);
        }
        // [args] because: see comment on reallyDeploy
        const deployRes = await parentCtc.reallyDeploy([args], value);
        debug(`${shad}: waitForInfo deployRes = ${JSON.stringify(deployRes)}`);
        const { transactionHash, ...deployInfo } = deployRes;
        info = deployInfo;
        _waitForInfo = async () => true;
        return { wait: async () => ({ transactionHash }) };
      };
    } else {
      _waitForInfo = async () => {
        info = await extractInfo(parentCtc);
        return true;
      };
    }

    let _ethersC = null;
    let last_block = null;
    const getC = async () => {
      if (_ethersC) { return _ethersC; }
      await _waitForInfo(true);
      await verifyContract(info, bin);
      debug(`${shad}: attach to creation at ${info.creation_block}`);
      last_block = info.creation_block;
      _ethersC = new ethers.Contract(info.address, ABI, networkAccount);
      return _ethersC;
    };
    const callC = async (funcName, args, value) => {
      if (parentCtc.reallyDeploy && funcName == `m1`) {
        return await _waitForInfo(true, args, value);
      } else {
        return (await getC())[funcName]([last_block, ...args], { value });
      }
    };

    let _getInfo = async () => {
      while (!await _waitForInfo(false)) {
        await Timeout.set(1);
      }
      _getInfo = async () => ({
        address: info.address,
        creation_block: info.creation_block,
        args: info.args,
        value: info.value,
        creator: info.creator,
      });
      return await _getInfo();
    };
    const getInfo = async () => {
      return await _getInfo();
    };

    const updateLast = o => { last_block = o.blockNumber; };

    const getEventData = (ethersC, ok_evt, ok_e) => {
      const ok_args_abi = ethersC.interface.getEvent(ok_evt).inputs;
      const { args } = ethersC.interface.parseLog(ok_e);
      const [ok_bal, ...ok_vals] = ok_args_abi.map(a => args[a.name]);

      return [ok_bal, ok_vals];
    };

    const iam = (some_addr) => {
      if (some_addr == address) {
        return address;
      } else {
        throw Error(`I should be ${some_addr}, but am ${address}`);
      }
    };

    const wait = async (delta) => {
      // Don't wait from current time, wait from last_block
      // XXX
      debug(`=====Waiting ${delta} from ${last_block}: ${address}`);
      const p = await waitUntilTime(add(last_block, delta));
      debug(`=====Done waiting ${delta} from ${last_block}: ${address}`);
      return p;
    };

    const sendrecv_top = async (label, funcNum, evt_cnt, tys, args, value, out_tys, timeout_delay, try_p) => {
      void(try_p, evt_cnt);
      return sendrecv(label, funcNum, tys, args, value, out_tys, timeout_delay);
    };

    // XXX: receive expected tys of output and use them to unmunge
    /* eslint require-atomic-updates: off */
    const sendrecv = async (label, funcNum, tys, args, value, out_tys, timeout_delay) => {
      // XXX use tys
      // TODO: support BigNumber delays?
      timeout_delay = toNumberMay(timeout_delay);
      const funcName = `m${funcNum}`;
      // https://github.com/ethereum/web3.js/issues/2077
      if (tys.length !== args.length) {
        throw Error(`tys.length (${tys.length}) !== args.length (${args.length})`);
      }
      const munged = args.map((m, i) => tys[i].munge(tys[i].canonicalize(m)));

      debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- START --- ${JSON.stringify(munged)}`);
      let block_send_attempt = (last_block || 0);
      let block_repeat_count = 0;
      while (!timeout_delay || block_send_attempt < (last_block || 0) + timeout_delay) {
        let r_maybe = false;

        debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- TRY`);
        try {
          const r_fn = await callC(funcName, munged, value);
          r_maybe = await r_fn.wait();
        } catch (e) {
          debug(e);
          // XXX What should we do...? If we fail, but there's no timeout delay... then we should just die
          await Timeout.set(1);
          const current_block = await getNetworkTimeNumber();
          if (current_block == block_send_attempt) {
            block_repeat_count++;
          }
          block_send_attempt = current_block;
          if ( /* timeout_delay && */ block_repeat_count > 32) {
            if (e.code === 'UNPREDICTABLE_GAS_LIMIT') {
              let error = e;
              while (error.error) { error = error.error; }
              console.log(`impossible: The message you are trying to send appears to be invalid.`);
              console.log(error);
            }
            console.log(`args:`);
            console.log(munged);
            throw Error(`${shad}: ${label} send ${funcName} ${timeout_delay} --- REPEAT @ ${block_send_attempt} x ${block_repeat_count}`);
          }
          debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- TRY FAIL --- ${last_block} ${current_block} ${block_repeat_count} ${block_send_attempt}`);
          continue;
        }

        assert(r_maybe != false);
        const ok_r = await fetchAndRejectInvalidReceiptFor(r_maybe.transactionHash);

        debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- OKAY`);

        // XXX It might be a little dangerous to rely on the polling to just work

        // It may be the case that the next line could speed things up?
        // last_block = ok_r.blockNumber;
        void(ok_r);

        return await recv(label, funcNum, out_tys, timeout_delay);
      }

      // XXX If we were trying to join, but we got sniped, then we'll
      // think that there is a timeout and then we'll wait forever for
      // the timeout message.

      debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- FAIL/TIMEOUT`);
      const rec_res = {};
      rec_res.didTimeout = true;
      return rec_res;
    };

    // XXX: receive expected tys of output and use them to unmunge
    const recv_top = async (label, okNum, ok_cnt, out_tys, timeout_delay) => {
      return recv(label, okNum, out_tys, timeout_delay);
    };

    // https://docs.ethers.io/ethers.js/html/api-contract.html#configuring-events
    const recv = async (label, okNum, out_tys, timeout_delay) => {
      // TODO: support BigNumber delays?
      timeout_delay = toNumberMay(timeout_delay);
      const ethersC = (await getC());
      const ok_evt = `e${okNum}`;
      debug(`${shad}: ${label} recv ${ok_evt} ${timeout_delay} --- START`);

      let block_poll_start = last_block;
      let block_poll_end = block_poll_start;
      while (!timeout_delay || block_poll_start < last_block + timeout_delay) {
        // console.log(
        //   `~~~ ${label} is polling [${block_poll_start}, ${block_poll_end}]\n` +
        //     `  ~ ${label} will stop polling at ${last_block} + ${timeout_delay} = ${last_block + timeout_delay}`,
        // );

        const es = await provider.getLogs({
          fromBlock: block_poll_start,
          toBlock: block_poll_end,
          address: ethersC.address,
          topics: [ethersC.interface.getEventTopic(ok_evt)],
        });
        if (es.length == 0) {
          debug(`${shad}: ${label} recv ${ok_evt} ${timeout_delay} --- RETRY`);
          block_poll_start = block_poll_end;

          await Timeout.set(1);
          void(ethersBlockOnceP); // This might be a better option too, because we won't need to delay
          block_poll_end = await getNetworkTimeNumber();

          continue;
        } else {
          debug(`${shad}: ${label} recv ${ok_evt} ${timeout_delay} --- OKAY`);

          const ok_e = es[0];

          const ok_r = await fetchAndRejectInvalidReceiptFor(ok_e.transactionHash);
          void(ok_r);
          const ok_t = await provider.getTransaction(ok_e.transactionHash);
          debug(`${ok_evt} gas was ${ok_t.gas} ${ok_t.gasPrice}`);

          updateLast(ok_t);
          const [ok_bal, ok_vals] = getEventData(ethersC, ok_evt, ok_e);
          if (ok_vals.length !== out_tys.length) {
            throw Error(`Expected ${out_tys.length} values from event data, but got ${ok_vals.length}.`);
          }
          const data = ok_vals.map((v, i) => out_tys[i].unmunge(v));

          debug(`${shad}: ${label} recv ${ok_evt} ${timeout_delay} --- OKAY --- ${JSON.stringify(ok_vals)}`);
          return { didTimeout: false, data, value: ok_t.value, balance: ok_bal, from: ok_t.from };
        }
      }

      debug(`${shad}: ${label} recv ${ok_evt} ${timeout_delay} --- TIMEOUT`);
      const rec_res = {};
      rec_res.didTimeout = true;
      return rec_res;
    };

    return { sendrecv: sendrecv_top, recv: recv_top, iam, wait, getInfo };
  };

  // https://docs.ethers.io/v5/api/contract/contract-factory/
  const deploy = async (bin) => {
    const { ABI, Bytecode, deployMode } = bin._Connectors.ETH;
    const factory = new ethers.ContractFactory(ABI, Bytecode, networkAccount);

    // args : Option(Array(string))
    // where Option is represented as [] for None, and [x] for Some(x)
    // In the Some case, the args are intentionally tupled into a single Array arg
    const reallyDeploy = async (args, value) => {
      debug(`${shad}: reallyDeploying with ${JSON.stringify([args, value])}`);
      const contract = await factory.deploy(...args, { value });
      // Wait for it to actually be deployed.
      const deploy_r = await contract.deployTransaction.wait();

      return {
        address: contract.address,
        creation_block: deploy_r.blockNumber,
        args,
        value,
        creator: address,
        transactionHash: deploy_r.transactionHash,
      };
    };

    let ctc = null;
    if (deployMode == `DM_firstMsg`) {
      debug(`${shad}: delaying deploy-ment`);
      ctc = { reallyDeploy };
    } else {
      ctc = await reallyDeploy([], 0);
    }

    return await attach(bin, ctc);
  };

  return { deploy, attach, networkAccount };
};

export const newAccountFromMnemonic = async (phrase) => {
  const provider = await getProvider();
  const networkAccount = ethers.Wallet.fromMnemonic(phrase).connect(provider);
  const acc = await connectAccount(networkAccount);
  return acc;
};

// async () => signer
const getSigner = (() => {
  const signerP = (async () => {
    if (isIsolatedNetwork) {
      const provider = await getProvider();
      return provider.getSigner();
    } else {
      // Signer may not be accessed on non-isolated networks
      return null;
    }
  })();

  return async () => {
    requireIsolatedNetwork('getSigner');
    return await signerP;
  };
})();

export const newTestAccount = async (startingBalance) => {
  debug(`newTestAccount(${startingBalance})`);
  requireIsolatedNetwork('newTestAccount');
  const provider = await getProvider();
  const signer = await getSigner();

  const networkAccount = ethers.Wallet.createRandom().connect(provider);
  const to = networkAccount.address;

  try {
    debug(`awaiting transfer: ${to}`);
    await transfer(signer, networkAccount, startingBalance);
    debug(`got transfer. awaiting connectAccount: ${to}`);
    const acc = await connectAccount(networkAccount);
    debug(`got connectAccount: ${to}`);
    return acc;
  } catch (e) {
    console.log(`Trouble with account ${to}`);
    throw e;
  }
};

const getNetworkTimeNumber = async () => {
  const provider = await getProvider();
  return await provider.getBlockNumber();
};

export const getNetworkTime = async () => {
  return bigNumberify(await getNetworkTimeNumber());
};

// onProgress callback is optional, it will be given an obj
// {currentTime, targetTime}
export const wait = async (delta, onProgress) => {
  const now = await getNetworkTime();
  return await waitUntilTime(add(now, delta), onProgress);
};

// onProgress callback is optional, it will be given an obj
// {currentTime, targetTime}
export const waitUntilTime = async (targetTime, onProgress) => {
  targetTime = bigNumberify(targetTime);
  if (isIsolatedNetwork) {
    await fastForwardTo(targetTime, onProgress);
  } else {
    await actuallyWaitUntilTime(targetTime, onProgress);
  }
};

// onProgress callback is optional, it will be given an obj
// {currentTime, targetTime}
const actuallyWaitUntilTime = async (targetTime, onProgress) => {
  onProgress = onProgress || (() => {});
  const provider = await getProvider();
  return await new Promise((resolve) => {
    const onBlock = async (currentTime) => {
      // Does not block on the progress fn if it is async
      onProgress({ currentTime, targetTime });
      if (ge(currentTime, targetTime)) {
        provider.off('block', onBlock);
        resolve(currentTime);
      }
    };
    provider.on('block', onBlock);

    // Also "re-emit" the current block
    // Note: this sometimes causes the starting block
    // to be processed twice, which should be harmless.
    getNetworkTime().then(onBlock);
  });
};

const fastForwardTo = async (targetTime, onProgress) => {
  // console.log(`>>> FFWD TO: ${targetTime}`);
  onProgress = onProgress || (() => {});
  requireIsolatedNetwork('fastForwardTo');
  let currentTime;
  while (lt(currentTime = await getNetworkTime(), targetTime)) {
    onProgress({ currentTime, targetTime });
    await stepTime();
  }
  // Also report progress at completion time
  onProgress({ currentTime, targetTime });
  // console.log(`<<< FFWD TO: ${targetTime} complete. It's ${currentTime}`);
};

const requireIsolatedNetwork = (label) => {
  if (!isIsolatedNetwork) {
    throw Error(`Invalid operation ${label} in REACH_CONNECTOR_MODE=${connectorMode}`);
  }
};

const dummyAccountP = (async () => {
  const provider = await getProvider();
  const networkAccount = ethers.Wallet.createRandom().connect(provider);
  const acc = await connectAccount(networkAccount);
  return acc;
})();

const stepTime = async () => {
  requireIsolatedNetwork('stepTime');
  const signer = await getSigner();
  const acc = await dummyAccountP;
  return await transfer(signer, acc, toWeiBigNumber('0', 'ether'));
};

const toNumberMay = (x) => {
  if (isBigNumber(x)) {
    return x.toNumber();
  } else {
    return x;
  }
};

// Check the contract info and the associated deployed bytecode;
// Verify that:
// * it matches the bytecode you are expecting.
// * it was deployed at exactly creation_block.
// Throws an Error if any verifications fail
export const verifyContract = async (ctcInfo, backend) => {
  const { ABI, Bytecode } = backend._Connectors.ETH;
  const { address, creation_block, args, value, creator } = ctcInfo;
  const factory = new ethers.ContractFactory(ABI, Bytecode);

  // TODO: is there a way to get the creation_block & bytecode with a single api call?
  // https://docs.ethers.io/v5/api/providers/provider/#Provider-getCode
  const provider = await getProvider();
  const nocode = await provider.getCode(address, creation_block - 1);
  if (nocode !== '0x') {
    throw Error(`Contract was deployed earlier than ${creation_block} (as was claimed)`);
  }
  const actual = await provider.getCode(address, creation_block);

  // see comment on reallyDeploy about args
  const deployData = factory.getDeployTransaction(...args).data;
  if (!deployData.startsWith(backend._Connectors.ETH.Bytecode)) {
    throw Error(`Impossible: contract with args is not prefixed by backend Bytecode`);
  }

  // FIXME this is based on empirical observation, feels hacky
  // deployData looks like this: [init][setup][body][teardown]
  // actual looks like this:     [init][body]
  // XXX the labels "init", "setup", and "teardown" are probably misleading
  // FIXME: for 0-arg contract deploys, it appears that:
  // * "init" is of length 13
  // * "setup" is not consistent in content, but is of length 156
  // * "teardown" is of length 0
  // FIXME: for n-arg contract deploys, it appears that:
  // * "init" is of length 13
  // * "setup" is of length >= 0 (and probably >= 156)
  // * "teardown" is of length >= 0
  const initLen = 13;
  const setupLen = 156;
  const expected = deployData.slice(0, initLen) + deployData.slice(initLen + setupLen);

  if (expected.length <= 0) {
    throw Error(`Impossible: contract expectation is empty`);
  }

  if (actual !== expected) {
    // FIXME: Empirical observation says that 0-arg contract deploys
    // should === expected. However, this is fragile (?), so it's ok
    // to only pass the next check.

    // FIXME: the 13-char header is also fragile, but we're just
    // running with that assumption for now.

    const deployNoInit = deployData.slice(initLen);
    const actualNoInit = actual.slice(initLen);
    if (actualNoInit.length === 0 || !deployNoInit.includes(actualNoInit)) {
      // FIXME: this display is not so helful for the n-arg contract deploy case.
      const displayLen = 60;
      console.log('--------------------------------------------');
      console.log('expected start: ' + expected.slice(0, displayLen));
      console.log('actual   start: ' + actual.slice(0, displayLen));
      console.log('--------------------------------------------');
      console.log('expected   end: ' + expected.slice(expected.length - displayLen));
      console.log('actual     end: ' + actual.slice(actual.length - displayLen));
      console.log('--------------------------------------------');
      console.log('expected   len: ' + expected.length);
      console.log('actual     len: ' + actual.length);
      console.log('--------------------------------------------');
      throw Error(`Contract bytecode does not match expected bytecode.`);
    }
  }

  const bal = await provider.getBalance(address, creation_block);
  // bal is allowed to exceed expectations, for example,
  // if someone spuriously transferred extra money to the contract
  if (!ge(bal, value)) {
    console.log('bal expected: ' + value);
    console.log('bal actual  : ' + bal);
    throw Error(`Contract initial balance does not match expected initial balance`);
  }

  if (args.length == 0) {
    const st = await provider.getStorageAt(address, 0, creation_block);
    const expectedSt = keccak256(0, creation_block);
    if (st !== expectedSt) {
      console.log('st expected: ' + expectedSt);
      console.log('st actual  : ' + st);
      throw Error(`Contract initial state does not match expected initial state.`);
    }
  } else {
    // TODO: figure out freeVars using creator and args
    void(creator);
    // const expectedSt = keccak256(1, creation_block, ...freeVars)
    // if st !== expectedSt throw Error
  }

  return true;
};
