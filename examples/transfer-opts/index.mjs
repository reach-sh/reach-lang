import {loadStdlib} from '@reach-sh/stdlib';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);

const [ accAlice, accBob, accCharlie ] =
  await stdlib.newTestAccounts(3, startingBalance);

if (stdlib.connector === 'ALGO') {
  const algos = n => n * 1000000
  const assertBal = async (acc, amt) => stdlib.assert(
    (await acc.balanceOf()).sub(algos(amt)).abs().lt(algos(0.2))
  );
  const assertBals = (a, b, c) => Promise.all([
    assertBal(accAlice, a),
    assertBal(accBob, b),
    assertBal(accCharlie, c),
  ]);
  
  await assertBals(100, 100, 100);
  const note = Uint8Array.from(Buffer.from('Here is 5 Algo :)'))
  await stdlib.transfer(accAlice, accBob, algos(5), undefined, { note });
  await assertBals(95, 105, 100);
  await stdlib.transfer(accAlice, accBob, algos(10), undefined, { closeTo: accCharlie.getAddress() });
  await assertBals(0, 115, 185);
} else {
  // there is nothing to test on ETH/CFX, as the opts just will be ignored
}
