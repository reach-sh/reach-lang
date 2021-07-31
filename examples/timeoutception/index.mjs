import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';

console.log(`getting started...`);
(async() => {
  const stdlib = await stdlib_loader.loadStdlib();

  const dispAmt = (x) => `${stdlib.formatCurrency(x)} ${stdlib.standardUnit}`;
  const getBalance = async (who) => dispAmt(await stdlib.balanceOf(who));

  const acc = await stdlib.newTestAccount(stdlib.parseCurrency(100));
  const beforeBal = await getBalance(acc);
  const beforeTime = await stdlib.getNetworkTime();
  console.log(`At ${beforeTime} A starts with ${beforeBal}`);

  const ask = async () => {
    await stdlib.wait(15, ({current, target}) => {
      if (current < target) {
        console.log(`At ${current} A is waiting for ${target - current} more...`);
      }
    });
    return 2;
  };
  const interact = {ask};

  const ctc = acc.deploy(backend);
  await backend.A(ctc, interact);

  const afterBal = await getBalance(acc);
  const afterTime = await stdlib.getNetworkTime();
  console.log(`At ${afterTime} A now has ${afterBal}`);
})();
