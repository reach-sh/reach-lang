import * as stdlib from '@reach-sh/stdlib/ALGO.mjs';
// import * as backend from './build/index.main.mjs';

(async () => {
  const startingBalance = stdlib.algosToMicroalgos(100);
  console.log(`Let us begin by giving each participant ${startingBalance} microalgos`);

  const showCurrency = (amt) =>
        `${stdlib.microalgosToAlgos(amt).toFixed(2)} ALGO`;
  const logBalance = async (who, acc) => {
    const bal = await stdlib.balanceOf(acc);
    console.log(`${who} has balance ${showCurrency(bal)}`);
  };

  console.log('creating test accounts');
  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);
  console.log('...created.');

  // const ctcAlice = await alice.deploy(backend);
  // const ctcBob = await bob.attach(backend, ctcAlice);

  // await Promise.all([
  //   backend.Alice(
  //     stdlib, ctcAlice,
  //     { ...stdlib.hasRandom }
  //   ),
  //   backend.Bob(
  //     stdlib, ctcBob,
  //     { ...stdlib.hasRandom }
  //   ),
  // ]);

  console.log('Hello, Alice and Bob!');
  await logBalance('Alice', alice);
  await logBalance('Bob', bob);

  await stdlib.transfer(alice, bob, stdlib.algosToMicroalgos(10));
  console.log('Alice transferred Bob some money.');

  await logBalance('Alice', alice);
  await logBalance('Bob', bob);
})();
