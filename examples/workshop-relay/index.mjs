import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as backend from './build/index.main.mjs';

(async () => {
  const startingBalance = stdlib.parseCurrency({ETH: 100});

  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const getBalance = async (who) =>
        stdlib.formatCurrency(await stdlib.balanceOf(who), 4);
  const beforeAlice = await getBalance(accAlice);
  const beforeBob = await getBalance(accBob);

  const ctcAlice = await accAlice.deploy(backend);

  let accRelayProvide = null;
  const accRelayP = new Promise((resolve, reject) => {
    accRelayProvide = resolve;
  });

  await Promise.all([
    backend.Alice(stdlib, ctcAlice, {
      amt: stdlib.parseCurrency({ETH: 25}),
      getRelay: async () => {
        console.log(`Alice creates a Relay account.`);
        const accRelay = await stdlib.newTestAccount(
          stdlib.parseCurrency({ETH: 0}));
        console.log(`Alice shares it with Bob outside of the network.`);
        accRelayProvide(accRelay);
        return accRelay.networkAccount.address;
      }
    }),
    (async () => {
      console.log(`Bob waits for Alice to give him the information about the Relay account.`);
      const accRelay = await accRelayP;
      console.log(`Bob deposits some funds into the Relay to use it.`);
      await stdlib.transfer(accBob, accRelay, stdlib.parseCurrency({ETH: 1}));
      console.log(`Bob attaches to the contract as the Relay.`);
      const ctcRelay = await accRelay.attach(backend, ctcAlice);
      console.log(`Bob joins the application as the Relay.`);
      return backend.Relay(stdlib, ctcRelay, {
        getBob: async () => {
          console.log(`Bob, acting as the Relay, gives his information.`);
          return accBob.networkAccount.address;
        },
      });
    })(),
  ]);

  const afterAlice = await getBalance(accAlice);
  const afterBob = await getBalance(accBob);

  console.log(`Alice went from ${beforeAlice} to ${afterAlice}.`);
  console.log(`Bob went from ${beforeBob} to ${afterBob}.`);

})();
