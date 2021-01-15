// This is the Reach-supplied RPC library
const rpc = async (m, ...args) => {
  console.log(`XXX RPC ${m} ${JSON.stringify(args)}`);
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
    var p = rpc(m, arg, vals, meths);
    while (true) {
      try {
        const r = await p;
        // XXX Look at r, if it is "callback", then call something in cbacks
        return resolve(r);
      } catch (e) {
        return reject(e);
      }
    }
  })());
};

// This is the thing a programmer would write
(async () => {
  console.log(`I am the client`);

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
    // XXX ...stdlib.hasRandom,
    getHand: async () => {
      const hand = Math.floor(Math.random() * 3);
      console.log(`${Who} played ${HAND[hand]}`);
      return hand;
    },
    seeOutcome: (outcome) => {
      console.log(`${Who} saw outcome ${OUTCOME[outcome]}`);
    },
    informTimeout: () => {
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
      acceptWager: (amt) => {
        console.log(`Bob accepts the wager of ${fmt(amt)}.`);
      },
    }),
  ]);

  const afterAlice = await getBalance(accAlice);
  const afterBob = await getBalance(accBob);

  console.log(`Alice went from ${beforeAlice} to ${afterAlice}.`);
  console.log(`Bob went from ${beforeBob} to ${afterBob}.`);

})();
