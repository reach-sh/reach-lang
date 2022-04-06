import chalk          from 'chalk';
import { mkRPC }      from '@reach-sh/rpc-client';
import { mkAssertEq } from './common.mjs';

const { rpc, rpcCallbacks } = await mkRPC();
const assertEq              = mkAssertEq(rpc);
const sbal                  = await rpc('/stdlib/parseCurrency', 200);
const [ accAlice, accBob ]  = await rpc('/stdlib/newTestAccounts', 2, sbal);

await rpc('/acc/setDebugLabel', accAlice, 'Alice');
await rpc('/acc/setDebugLabel', accBob,   'Bob');

const c = await rpc('/stdlib/connector');
if (c === 'ETH' || c === 'CFX') {
  await Promise.all(
    [ rpc('/acc/setGasLimit', accAlice, 5000000)
    , rpc('/acc/setGasLimit', accAlice, 5000000) ]);
}

const ctcAlice = await rpc('/acc/contract', accAlice);
const ctcBob   = rpc('/ctc/getInfo', ctcAlice).then(i => rpc('/acc/contract', accBob, i));

const fmt = async x => await rpc('/stdlib/formatCurrency', x, 4);

const go = async ({ role, ctc, acc }) => {
  const r = chalk.green.bold(role);
  let tok = null;

  const showBalance = async () => {
    const b = await fmt(await rpc('/stdlib/balanceOf', acc, tok));
    console.log(`${r}: ${JSON.stringify(tok)} balance: ${b}.`);
  };

  const showToken = async (_tok, cmd) => {
    tok = _tok;
    console.log(`${r}: The token is: ${JSON.stringify(tok)}.`);
    await showBalance();
    const omd = await rpc('/acc/tokenMetadata', acc, tok);
    for (const f in cmd) {
      assertEq(`${role.trimStart()}: computed ${f} should match on-chain ${f}:`, cmd[f], omd[f]);
    }
    console.log(`${r}: Opt-in to ${JSON.stringify(tok)}:`);
    await rpc('/acc/tokenAccept', acc, tok);
    await showBalance();
  };

  let amt = null;
  const didTransfer = async (did, _amt) => {
    if (did) {
      amt = _amt;
      const a = await fmt(amt);
      console.log(`${r}: Received transfer of ${a} for ${JSON.stringify(tok)}.`);
    }
    await showBalance();
  };

  const getParams = async () => ({
    name:     'Gil',
    symbol:   'GIL',
    url:      'https://tinyurl.com/4nd2faer',
    metadata: 'It\'s shiny!',
    supply:   await rpc('/stdlib/parseCurrency', 1000),
    amt:      await rpc('/stdlib/parseCurrency',   10),
  });

  const io = {
    getParams,
    showToken,
    didTransfer,
  };

  console.log(`${r}: Starting backend...`);
  await rpcCallbacks(`/backend/${role.trimStart()}`, ctc, io);
  console.log(`${r}: Done.`);
};

await Promise.all([
  go({ role: 'Alice', ctc: await ctcAlice, acc: accAlice, }),
  go({ role: '  Bob', ctc: await ctcBob,   acc: accBob,   }),
]);

await Promise.all([
  rpc('/forget/ctc', ctcAlice, ctcBob),
  rpc('/forget/acc', accAlice, accBob),
]);
