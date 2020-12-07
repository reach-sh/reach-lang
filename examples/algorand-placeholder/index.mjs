import * as stdlibLoader from '@reach-sh/stdlib/loader.mjs';
// import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await stdlibLoader.loadStdlib();
  const connector = stdlibLoader.getConnector();

  // Very rough approximation of value of 1 algo
  const proportionalToAlgo = (x) => stdlib.parseCurrency(
    connector === 'ETH' ? x / 1000
      : connector === 'ALGO' ? x
      : x * 5 // Disclaimer: this is pretend; FAKE has no worth
  );

  const startingBalance = proportionalToAlgo(1000);

  const decimals = connector === 'FAKE' ? 0 : 2;
  const showCurrency = (amt) =>
        `${stdlib.formatCurrency(amt, decimals)} ${stdlib.standardUnit}`;
  const logBalance = async (who, acc) => {
    const bal = await stdlib.balanceOf(acc);
    console.log(`${who} has balance ${showCurrency(bal)}`);
  };

  console.log(`Let us begin by giving each participant ${showCurrency(startingBalance)}`);
  console.log('creating test accounts');
  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);
  console.log('...created.');

  // const ctcAlice = alice.deploy(backend);
  // const ctcBob = bob.attach(backend, ctcAlice.getInfo());

  // await Promise.all([
  //   backend.Alice(
  //     ctcAlice,
  //     { ...stdlib.hasRandom }
  //   ),
  //   backend.Bob(
  //     ctcBob,
  //     { ...stdlib.hasRandom }
  //   ),
  // ]);

  console.log('Hello, Alice and Bob!');
  await logBalance('Alice', alice);
  await logBalance('Bob', bob);

  await stdlib.transfer(alice, bob, proportionalToAlgo(100));
  console.log('Alice transferred Bob some money.');

  await logBalance('Alice', alice);
  await logBalance('Bob', bob);
})();
