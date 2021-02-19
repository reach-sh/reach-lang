import { mkRPC } from '@reach-sh/rpc-client';

(async () => {
  const { rpcReady, rpc, rpcCallbacks } = mkRPC();

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

  // await rpc(`/stop`);
})();
