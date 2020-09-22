import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as backend from './build/index.main.mjs';

const parseEth = (n) => stdlib.parseCurrency({ETH: n});
const dispAmt = (x) => `${stdlib.formatCurrency(x)} ${stdlib.standardUnit}`;
const getBalance = async (who) => dispAmt(await stdlib.balanceOf(who));

console.log(`getting started...`);
(async() => {
  const acc = await stdlib.newTestAccount(parseEth('100'));
  const beforeBal = await getBalance(acc);
  const beforeTime = await stdlib.getNetworkTime();
  console.log(`At ${beforeTime} A starts with ${beforeBal}`);

  const ask = async () => {
    await stdlib.wait(15, ({currentTime, targetTime}) => {
      if (currentTime < targetTime) {
        console.log(`At ${currentTime} A is waiting for ${targetTime - currentTime} more...`);
      }
    });
    return 2;
  };
  const interact = {ask};

  const ctc = await acc.deploy(backend);
  await backend.A(stdlib, ctc, interact);

  const afterBal = await getBalance(acc);
  const afterTime = await stdlib.getNetworkTime();
  console.log(`At ${afterTime} A now has ${afterBal}`);
})();
