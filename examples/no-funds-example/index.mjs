import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const theLog = [];
function log(m) {
  theLog.push(m);
  console.log(m);
}

function doTheThing(accDeployer, accAttacher) {
  log('Hello, Deployer and Attacher!');

  log('Launching...');
  const ctcDeployer = accDeployer.deploy(backend);
  const ctcAttacher = accAttacher.attach(backend, ctcDeployer.getInfo());

  log('Starting backends...');

  const pDeployer = backend.Deployer(ctcDeployer, {deadline: 10});
  const pAttacher = backend.Attacher(ctcAttacher, {});
  const pDone = Promise.all([pDeployer, pAttacher]).then(() => {
    log('Goodbye!');
  }).catch(() => {
    log('Goodbye! (error occured)');
  });

  return {pDeployer, pAttacher, pDone};
}

(async () => {
  const startingBalance = stdlib.parseCurrency(100);
  const zero = stdlib.parseCurrency(0);

  const [ accDeployer, accAttacher ] =
    await stdlib.newTestAccounts(2, startingBalance);
  const [ accDeployer0, accAttacher0 ] =
    await stdlib.newTestAccounts(2, zero);

  await (async () => {
    const label = 'normal';
    log(`\n'${label}': start`);
    const {pDone} = doTheThing(accDeployer, accAttacher);
    await pDone;
    log(`'${label}': didn't throw, as expected.`);
  })();

  await (async () => {
    const label = 'attacher0';
    log(`\n'${label}': start`);

    const {pAttacher, pDone} = doTheThing(accDeployer, accAttacher0);
    if (!pAttacher || !pDone) {
      log(`pAttacher or pDone is empty`);
      process.exit(1);
    }
    let e = null;
    log(`waiting for pAttacher to resolve or reject`);
    try {
      await pAttacher;
    } catch (_e) {
      e = _e;
    }
    log(`done waiting on pAttacher`);
    try {
      const before = (await stdlib.getNetworkTime()).toNumber();
      log(`waiting...`, {before});
      await stdlib.wait(10); // make the timeout elapse so that Deployer can complete the contract
      const after = (await stdlib.getNetworkTime()).toNumber();
      log(`done waiting.`, {before, after});
      await pDone;
      log(`pDone is done`);
    } catch (_e) {
      log('!!! error waiting');
      log(_e);
      process.exit(1);
    }
    if (e) {
      log(`'${label}' threw, as expected.`);
    } else {
      log(`'${label}' didn't throw, but it should have.`);
      process.exit(1);
    }
  })();

  // TODO: also test what happens with (accDeployer0, accAttacher),
  // when Deployer doesn't have enough money
  // Not sure if the following may be needed

  // XXX this is questionable
  // process.on('unhandledRejection', (e) => {
  //   log(`squelching 'unhandled' rejection`);
  // });
  // process.on('uncaughtException', (e) => {
  //   log(`squelching 'unhandled' exception`);
  // });

  const expectedLog = ["\n'normal': start","Hello, Deployer and Attacher!","Launching...","Starting backends...","Goodbye!","'normal': didn't throw, as expected.","\n'attacher0': start","Hello, Deployer and Attacher!","Launching...","Starting backends...","waiting for pAttacher to resolve or reject","done waiting on pAttacher","Goodbye! (error occured)","waiting...","done waiting.","pDone is done","'attacher0' threw, as expected."];
  const logfail = () => {
      console.log(`theLog doesn't match expectedLog`);
      console.log(`expected`);
      console.log(JSON.stringify(expectedLog));
      console.log(`actual`);
      console.log(JSON.stringify(theLog));
      process.exit(1);
  }
  if (expectedLog.length !== theLog.length) { logfail(); }
  for (const i in expectedLog) {
    if (theLog[i] !== expectedLog[i]) { logfail(); }
  }
  console.log(`donezo`);
  process.exit(0);
})();
