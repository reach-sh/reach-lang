import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import * as listenerBackend from './build/announcer.main.mjs';
import * as ask from '@reach-sh/stdlib/ask.mjs';
import { runManager } from './announcer.mjs';
import { runTokens } from './tokens.mjs';

const stdlib = await loadStdlib();
const startingBalance = stdlib.parseCurrency(9999);

// Track who withdrew/deposited
const withdrew  = {};
const deposited = {};
const traded = {};

const isZmdOrGil = (ans) => {
  if (ans.toLowerCase() == 'zmd') {
    return 'ZMD';
  }
  if (ans.toLowerCase() == 'gil') {
    return 'GIL';
  }
  throw Error('Only `ZMD` or `GIL` are valid answers.');
}

const fmt = (x) => stdlib.formatCurrency(x, 4);

const getBalance = async (tokenX, who) => {
  const amt = await stdlib.balanceOf(who, tokenX.id);
  return `${fmt(amt)} ${tokenX.sym}`; };

const getBalances = async (who, zmd, gil) =>
  `${await getBalance(zmd, who)} & ${await getBalance(gil, who)}`;

const runDuoSwapAdmin = async () => {

  // Create & Fund Admin
  const accAdmin = await stdlib.newTestAccount(startingBalance)
  await accAdmin.setDebugLabel('Admin');
  const _ = await ask.ask(`Fund: ${accAdmin.getAddress()}`);
  const res = await ask.ask(`Enter token info:`, JSON.parse);
  const zmd = { id: res.zmd, sym: 'zmd', name: 'zorkmid' };
  const gil = { id: res.gil, sym: 'gil', name: 'gil' };

  // Deploy contract
  const ctcAdmin = accAdmin.deploy(backend);
  const ctcInfo = ctcAdmin.getInfo();
  const listenerInfo = await ask.ask(`Paste announcer ctc info:`);
  const ctcListener = accAdmin.attach(listenerBackend, listenerInfo)
  const poolAddr = (await ctcInfo).toString();
  const connectionInfo = { poolAddr, zmd, gil };
  console.log(`Connection Info: `, JSON.stringify(connectionInfo));

  // Admin backend
  const adminBackend = backend.Admin(ctcAdmin, {
    tokA: zmd.id,
    tokB: gil.id,
    shouldClosePool: async (_) => {
      const answer = await ask.ask(`Do you want to close the pool? (y/n)`, ask.yesno);
      return { when: answer, msg: null };
    },
  });

  // Pool info for sharing
  let sharedPool = false;
  const backendListener = listenerBackend.Listener(ctcListener, {
    hear: (poolInfo) => {
      console.log(`Listener hears!`, poolInfo);
      // Do not share pool again once shared
      if (poolInfo = poolAddr) {
        sharedPool = true;
      }
    },
    getPoolInfo: () => {
      return [ !sharedPool, poolAddr ]
    },
  });

  await Promise.all([ backendListener, adminBackend ]);
};

const runDuoSwapLP = async () => {
  // Create & Fund Admin
  const accProvider = await stdlib.newTestAccount(startingBalance)
  await accProvider.setDebugLabel('Provider');
  if (stdlib.connector == 'ETH') {
    accProvider.setGasLimit(5000000);
  }
  const _ = await ask.ask(`Fund: ${accProvider.getAddress()}`);
  const { zmd, gil, poolAddr } = await ask.ask(`Enter connection info:`, JSON.parse);
  const ctcProvider = accProvider.attach(backend, poolAddr);

  const backendProvider = backend.Provider(ctcProvider, {
    log: (s, x) => { console.log(s.padStart(30), x.toString()); },
    withdrawDone: (isMe, amtOuts) => {
      if (isMe) {
        withdrew[accProvider] = true;
        console.log("\x1b[31m", `I withdrew ${amtOuts[0]} ZMD & ${amtOuts[1]} GIL`,'\x1b[0m');
      }
    },
    withdrawMaybe: async ([ alive, market ]) => {
      const wantsToWithdraw = await ask.ask(`Do you want to withdraw liquidity? (y/n)`, ask.yesno);
      if (wantsToWithdraw) {
        const amt = await ask.ask(`How much liquidity do you want to withdraw?`, parseInt);
        return { when: true, msg: { liquidity: amt } };
      } else {
        return { when: false, msg: { liquidity: 0 }};
      }
    },
    depositDone: (isMe, amtA, amtB, poolTokens) => {
      if (isMe) {
        deposited[accProvider] = poolTokens;
        console.log("\x1b[34m", `I received ${poolTokens} pool tokens for their deposit of ${amtA} ZMD & ${amtB} GIL`,'\x1b[0m');
      }
    },
    depositMaybe: async ([ isAlive, market ]) => {
      const wantsToDeposit = await ask.ask(`Do you want to deposit? (y/n)`, ask.yesno);
      if (wantsToDeposit) {
        const myBals = await getBalances(accProvider, zmd, gil);
        const amtA = await ask.ask(`How much ZMD do you want to deposit? (Bal: ${myBals})`, parseInt);
        const amtB = await ask.ask(`How much GIL do you want to deposit? (Bal: ${myBals})`, parseInt);
        const deposit = { amtA: stdlib.parseCurrency(amtA), amtB: stdlib.parseCurrency(amtB) }
        return {
          when: true, msg: deposit
        };
      } else {
        return { when: false, msg: { amtA: 0, amtB: 0 }};
      }
    },
  });

  await Promise.all([ backendProvider ]);
}

const runDuoSwapTrader = async () => {
  // Create & Fund Admin
  const accTrader = await stdlib.newTestAccount(startingBalance)
  await accTrader.setDebugLabel('Trader');
  if (stdlib.connector == 'ETH') {
    accTrader.setGasLimit(5000000);
  }
  // Attach to tokens that Admin launched
  const _ = await ask.ask(`Fund: ${accTrader.getAddress()}`);
  const { zmd, gil, poolAddr } = await ask.ask(`Enter connection info:`, JSON.parse);
  const ctcTrader = accTrader.attach(backend, poolAddr);

  const backendTrader = backend.Trader(ctcTrader, {
    log: (s, x) => { console.log(s.padStart(30), x.toString()); },
    tradeDone: (isMe, [amtIn, amtInTok, amtOut, amtOutTok]) => {
      const tokIn  = amtInTok == zmd.id ? zmd : gil;
      const tokOut = amtOutTok == zmd.id ? zmd : gil;
      if (isMe) {
        traded[accTrader] = true;
        console.log("\x1b[32m", `I traded ${amtIn} ${tokIn.sym} for ${amtOut} ${tokOut.sym}`,'\x1b[0m');
      }
    },
    tradeMaybe: async ([ alive, market ]) => {
      const wantsToTrade = await ask.ask(`Do you want to trade? (y/n)`, ask.yesno);
      if (wantsToTrade) {
        const options = ['ZMD', 'GIL'].join('\n');
        const tokType = await ask.ask(`What token do you want to input?\n${options}`, isZmdOrGil);
        const myBal = await getBalance(tokType === 'ZMD' ? zmd : gil, accTrader);
        const amt = await ask.ask(`How much do you want to trade? (You have ${myBal})`);
        const trade =
          (tokType == 'ZMD')
            ? ({ amtA: stdlib.parseCurrency(amt), amtB: 0, amtInTok: zmd.id })
            : ({ amtA: 0, amtB: stdlib.parseCurrency(amt), amtInTok: gil.id });
        return { when: true, msg: trade };
      } else {
        return { when: false, msg: { amtA: 0, amtB: 0, amtInTok: zmd.id }};
      }
    },
  });

  await Promise.all([ backendTrader ]);
}

const options = [
  '1: DuoSwap Admin',
  '2: DuoSwap Liquidity Provider',
  '3: DuoSwap Trader',
  '4: DuoSwap Announcer',
  '5: DuoSwap Tokens',
].join('\n');

const answer = await ask.ask(`Who are you?\n${options}`, parseInt);

if (answer == 1) {
  // Creates a pool and sends info to announcer/cache
  await runDuoSwapAdmin();
}
if (answer == 2) {
  await runDuoSwapLP();
}
if (answer == 3) {
  await runDuoSwapTrader();
}
if (answer == 4) {
  await runManager();
}
if (answer == 5) {
  await runTokens();
}
