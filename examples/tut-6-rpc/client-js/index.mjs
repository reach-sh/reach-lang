// This is the Reach-supplied RPC library
import waitPort from 'wait-port';
import bent from 'bent';

const rpc_server = process.env.REACH_RPC_SERVER;
const rpc_port = process.env.REACH_RPC_PORT;
const call = bent(`http://${rpc_server}:${rpc_port}`, `POST`, `json`, 200);

const rpcReady = async () => {
  await waitPort({host: rpc_server, port: parseInt(rpc_port)}); };

const rpc = async (m, ...args) => {
  const lab = `RPC ${m} ${JSON.stringify(args)}`
  console.log(`${lab}`);
  const ans = await call(m, args);
  console.log(`${lab} ==> ${JSON.stringify(ans)}`);
  return ans;
};

const rpcCallbacks = async (m, arg, cbacks) => {
  const vals = {};
  const meths = {};
  for (const k in cbacks) {
    const v = cbacks[k];
    if ( v instanceof Function ) {
      meths[k] = true;
    } else {
      vals[k] = v;
    }
  }
  return new Promise((resolve, reject) => (async () => {
    let p = rpc(m, arg, vals, meths);
    while (true) {
      try {
        const r = await p;
        switch ( r.t ) {
          case 'Done': {
            return resolve(r.ans);
          }
          case 'Kont': {
            const { kid, m, args } = r;
            const ans = await cbacks[m](...args);
            p = rpc(`/kont`, kid, ans);
            break;
          }
          default:
            throw new Error(`Illegal callback return: ${JSON.stringify(r)}`);
        }
      } catch (e) {
        return reject(e);
      }
    }
  })());
};

// This is the thing a programmer would write
(async () => {
  console.log(`I am the client`);
  await rpcReady();

  const startingBalance = await rpc(`/stdlib/parseCurrency`, 10);
  const accAlice = await rpc(`/stdlib/newTestAccount`, startingBalance);
  const accBob = await rpc(`/stdlib/newTestAccount`, startingBalance);

  const fmt = async (x) => await rpc(`/stdlib/formatCurrency`, x, 4);
  const getBalance = async (who) => fmt(await rpc(`/stdlib/balanceOf`, who));
  const beforeAlice = await getBalance(accAlice);
  const beforeBob = await getBalance(accBob);

  const ctcAlice = await rpc(`/acc/deploy`, accAlice);
  const ctcBob = await rpc(`/acc/attach`, accBob, await rpc(`/ctc/getInfo`, ctcAlice));

  const HAND = ['Rock', 'Paper', 'Scissors'];
  const OUTCOME = ['Bob wins', 'Draw', 'Alice wins'];
  const Player = (Who) => ({
    "stdlib.hasRandom": true,
    getHand: async () => {
      const hand = Math.floor(Math.random() * 3);
      console.log(`${Who} played ${HAND[hand]}`);
      return hand;
    },
    seeOutcome: async (outcomeBN) => {
      const outcome = await rpc(`/stdlib/bigNumbertoNumber`, outcomeBN);
      console.log(`${Who} saw outcome ${OUTCOME[outcome]}`);
    },
    informTimeout: async () => {
      console.log(`${Who} observed a timeout`);
    },
  });

  await Promise.all([
    rpcCallbacks(`/backend/Alice`, ctcAlice, {
      ...Player('Alice'),
      wager: await rpc(`/stdlib/parseCurrency`, 5),
    }),
    rpcCallbacks(`/backend/Bob`, ctcBob, {
      ...Player('Bob'),
      acceptWager: async (amt) => {
        console.log(`Bob accepts the wager of ${await fmt(amt)}.`);
      },
    }),
  ]);

  const afterAlice = await getBalance(accAlice);
  const afterBob = await getBalance(accBob);

  console.log(`Alice went from ${beforeAlice} to ${afterAlice}.`);
  console.log(`Bob went from ${beforeBob} to ${afterBob}.`);

  await rpc(`/quit`);
})();
