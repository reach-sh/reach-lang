import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as backend from './build/index.main.mjs';

(async () => {
  const startingBalance = stdlib.toWeiBigNumber('100', 'ether');

  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const getBalance = async (who) =>
        stdlib.fromWei ( await stdlib.balanceOf(who) );
  const beforeAlice = await getBalance(accAlice);
  const beforeBob = await getBalance(accBob);

  const ctcAlice = await accAlice.deploy(backend);
  const ctcBob = await accBob.attach(backend, ctcAlice);

  const thePass = stdlib.randomUInt256();

  await Promise.all([
    backend.Alice(
      stdlib, ctcAlice,
      { amt: stdlib.toWeiBigNumber('25', 'ether'),
        pass: thePass } ),
    backend.Bob(
      stdlib, ctcBob,
      { getPass: (hpass) => {
        console.log(`Bob asked to give the preimage of ${hpass}.`);
        console.log(`Returning: ${thePass}`);
        return thePass; } } )
  ]);

  const afterAlice = await getBalance(accAlice);
  const afterBob = await getBalance(accBob);

  console.log(`Alice went from ${beforeAlice} to ${afterAlice}.`);
  console.log(`Bob went from ${beforeBob} to ${afterBob}.`);

})();
