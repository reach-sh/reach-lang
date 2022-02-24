import { mkRPC } from '@reach-sh/rpc-client';

const { rpc, rpcCallbacks } = await mkRPC();

const startingBalance = await rpc(`/stdlib/parseCurrency`,  100);
const accAlice        = await rpc(`/stdlib/newTestAccount`, startingBalance);
const accBob          = await rpc(`/stdlib/newTestAccount`, startingBalance);

const fmt = async x =>
  await rpc(`/stdlib/formatCurrency`, x, 4);

const getBalance = async who =>
  fmt(await rpc(`/stdlib/balanceOf`, who));

const beforeAlice = await getBalance(accAlice);
const beforeBob   = await getBalance(accBob);

const ctcAlice    = await rpc(`/acc/contract`, accAlice);

const HAND        = ['Rock', 'Paper', 'Scissors'];
const OUTCOME     = ['Bob wins', 'Draw', 'Alice wins'];

const Player = who => ({
  "stdlib.hasRandom": true,
  getHand: async () => {
    const hand = Math.floor(Math.random() * 3);
    console.log(`${who} played ${HAND[hand]}`);
    return hand;
  },
  seeOutcome: async (outcomeBN) => {
    const outcome = await rpc(`/stdlib/bigNumbertoNumber`, outcomeBN);
    console.log(`${who} saw outcome ${OUTCOME[outcome]}`);
  },
  informTimeout: async () => {
    console.log(`${who} observed a timeout`);
  },
});

await Promise.all([
  rpcCallbacks(`/backend/Alice`, ctcAlice, {
    ...Player('Alice'),
    wager: await rpc(`/stdlib/parseCurrency`, 5),
    deadline: 10,
  }),

  rpc(`/ctc/getInfo`, ctcAlice).then(async (info) => {
    const ctcBob = await rpc(`/acc/contract`, accBob, info);
    await rpcCallbacks(`/backend/Bob`, ctcBob, {
      ...Player('Bob'),
      acceptWager: async (amt) => {
        console.log(`Bob accepts the wager of ${await fmt(amt)}.`);
      },
    });
    return await rpc(`/forget/ctc`, ctcBob);
  }),
]);

const afterAlice = await getBalance(accAlice);
const afterBob   = await getBalance(accBob);

console.log(`Alice went from ${beforeAlice} to ${afterAlice}.`);
console.log(`  Bob went from ${beforeBob} to ${afterBob}.`);

await Promise.all([
  rpc(`/forget/acc`, accAlice, accBob),
  rpc(`/forget/ctc`, ctcAlice),
]);
