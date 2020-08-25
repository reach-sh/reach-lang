import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as backend from './build/index.main.mjs';

const parseEth = (n) => stdlib.toWeiBigNumber(n, 'ether');
const getBalance = async (who) => stdlib.fromWei ( await stdlib.balanceOf(who) );

console.log(`getting started...`);
(async() => {
  const acc = await stdlib.newTestAccount(parseEth('10'));
  const beforeBal = await getBalance(acc);
  console.log(`A starts with ${beforeBal}`);

  const ask = async () => {
    for (let i = 0; i < 15; i++) {
      console.log(`acc keys: ${Object.keys(acc)}`);
      console.log(`A delays...`);
      await stdlib.transfer(acc.networkAccount, acc.networkAccount, parseEth('0.1'));
    }
    return 2;
  };
  const interact = {ask};

  const ctc = await acc.deploy(backend);
  await backend.A(stdlib, ctc, interact);

  const afterBal = await getBalance(acc);
  console.log(`A now has ${afterBal}`);
})();
