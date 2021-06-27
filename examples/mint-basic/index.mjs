import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const assertEq = (expected, actual) => {
    const exps = JSON.stringify(expected);
    const acts = JSON.stringify(actual);
    console.log('assertEq', {expected, actual}, {exps, acts});
    stdlib.assert(exps === acts) };
  const startingBalance = stdlib.parseCurrency(10);
  const [ accAlice, accBob ] = await Promise.all([
    stdlib.newTestAccount(startingBalance),
    stdlib.newTestAccount(startingBalance),
  ]);
  if ( stdlib.connector == 'ETH' ) {
    const myGasLimit = 5000000;
    accAlice.setGasLimit(myGasLimit);
    accBob.setGasLimit(myGasLimit);
  }
  const ctcAlice = accAlice.deploy(backend);
  const ctcBob = accBob.attach(backend, ctcAlice.getInfo());

  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const go = async ({me, role, ctc, acc, other}) => {
    let tok = null;
    const showBalance = async () => {
      console.log(`${me}: Checking ${tok} balance:`);
      console.log(`${me}: ${tok} balance: ${fmt(await stdlib.balanceOf(acc, tok))}`);
    };
    const showToken = async (_tok) => {
      tok = _tok;
      console.log(`${me}: The token is: ${tok}`);
      await showBalance();
      console.log(`${me}: The token metadata is:`, await stdlib.tokenMetadata(tok));
      console.log(`${me}: Opt-in to ${tok}:`);
      await acc.tokenAccept(tok);
      await showBalance();
    };
    let amt = null;
    const didTransfer = async (did, _amt) => {
      if ( did ) {
        amt = _amt;
        console.log(`${me}: Received transfer of ${fmt(amt)} for ${tok}`);
      }
      await showBalance();
      console.log(`${me}: Doing transfer for ${tok}`);
      // This next line is weird.
      await stdlib.transfer(acc, other, amt, tok);
      await showBalance();
    };
    const getParams = () => ({
      name: `Gil`, symbol: `GIL`,
      url: `https://tinyurl.com/4nd2faer`,
      metadata: `It's shiny!`,
      supply: stdlib.parseCurrency(1000),
      amt: stdlib.parseCurrency(10),
      doEarlyTransfer: false,
    });
    const io = {
      getParams,
      showToken,
      didTransfer,
      assertEq,
    };
    console.log(`${me}: Starting backend...`);
    await role(ctc, io);
    console.log(`${me}: Done...`);
    await didTransfer(false, amt);
    console.log(`${me}: Really done`);
  };

  await Promise.all([
    go({me: `Alice`, role: backend.Alice, ctc: ctcAlice, acc: accAlice, other: accBob}),
    go({me: `Bob`, role: backend.Bob, ctc: ctcBob, acc: accBob, other: accAlice}),
  ]);

})();
