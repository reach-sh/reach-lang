import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';
import launchToken from '@reach-sh/stdlib/launchToken.mjs';

(async () => {
  const stdlib = await stdlib_loader.loadStdlib();
  const conn = stdlib_loader.getConnector();
  const startingBalance = stdlib.parseCurrency(10);
  const accCreator = await stdlib.newTestAccount(startingBalance);

  const zorkmid = await launchToken(stdlib, accCreator, "zorkmid", "ZMD");
  const gil = await launchToken(stdlib, accCreator, "gil", "GIL");

  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);
  if ( conn == 'ETH' ) {
    const myGasLimit = 5000000;
    accAlice.setGasLimit(myGasLimit);
    accBob.setGasLimit(myGasLimit);
  } else if ( conn == 'ALGO' ) {
    await stdlib.transfer(accAlice, accAlice, 0, zorkmid.id);
    await stdlib.transfer(accAlice, accAlice, 0, gil.id);
    await stdlib.transfer(accBob, accBob, 0, zorkmid.id);
    await stdlib.transfer(accBob, accBob, 0, gil.id);
  }

  for ( const acc of [ accAlice, accBob ] ) {
    await zorkmid.mint(acc, startingBalance.mul(2));
    await gil.mint(acc, startingBalance.mul(2));
  }

  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const doSwap = async (tokenA, amtA, tokenB, amtB, trusted) => {
    console.log(`\nPerforming swap of ${fmt(amtA)} ${tokenA.sym} for ${fmt(amtB)} ${tokenB.sym}`);

    const getBalance = async (tokenX, who) => {
      const amt = await stdlib.balanceOf(who, tokenX.id);
      return `${fmt(amt)} ${tokenX.sym}`; };
    const getBalances = async (who) =>
      `${await getBalance(tokenA, who)} & ${await getBalance(tokenB, who)}`;

    const beforeAlice = await getBalances(accAlice);
    const beforeBob = await getBalances(accBob);
    console.log(`Alice has ${beforeAlice}`);
    console.log(`Bob has ${beforeBob}`);

    if ( trusted ) {
      console.log(`Alice transfers to Bob honestly`);
      await stdlib.transfer(accAlice, accBob, amtA, tokenA.id);
      console.log(`Bob transfers to Alice honestly`);
      await stdlib.transfer(accBob, accAlice, amtB, tokenB.id);
    } else {
      console.log(`Alice will deploy the Reach DApp.`);
      const ctcAlice = accAlice.deploy(backend);
      console.log(`Bob attaches to the Reach DApp.`);
      const ctcBob = accBob.attach(backend, ctcAlice.getInfo());

      await Promise.all([
        backend.Alice(ctcAlice, {
          getSwap: () => {
            console.log(`Alice proposes one side of swap`);
            return [ tokenA.id, amtA, 10 ]; },
          confirm: (...v) => {
            const res = Math.random() < 0.5;
            console.log(`Alice confirms ${JSON.stringify(v)} with ${res}`);
            return res; },
        }),
        backend.Bob(ctcBob, {
          accSwap: (...v) => {
            console.log(`Bob propose other side of ${JSON.stringify(v)}`);
            return [ tokenB.id, amtB ]; },
        }),
      ]);
    }

    const afterAlice = await getBalances(accAlice);
    const afterBob = await getBalances(accBob);
    console.log(`Alice went from ${beforeAlice} to ${afterAlice}`);
    console.log(`Bob went from ${beforeBob} to ${afterBob}`);
  };

  const amtA = stdlib.parseCurrency(1);
  const amtB = stdlib.parseCurrency(2);

  await doSwap(zorkmid, amtA, gil, amtB, false);
  await doSwap(gil, amtB, zorkmid, amtA, false);
  await doSwap(zorkmid, amtA, gil, amtB, true);
})();
