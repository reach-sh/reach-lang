import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await stdlib_loader.loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);
  const smallest = stdlib.parseCurrency(1);

  const parent = await stdlib.newTestAccount(startingBalance);
  const parentCtc = parent.deploy(backend);
  console.log(`Parent deploys the contract.`);
  const parentInteract = {
    allowance: () => {
      const amt = stdlib.parseCurrency(50);
      console.log(`Parent deposits ${stdlib.formatCurrency(amt)}`);
      return amt;
    },
    approve: (howMuch, balance) => {
      const ans = stdlib.le(balance, smallest) || stdlib.lt(howMuch, stdlib.div(balance, 2));
      console.log(`Parent answers ${ans} to request for ${stdlib.formatCurrency(howMuch, 4)}`);
      return ans;
    },
  };
  const parentP = backend.Parent(stdlib, parentCtc, parentInteract);

  const child = await stdlib.newTestAccount(startingBalance);
  const childCtc = child.attach(backend, parentCtc.getInfo());
  const UNITS = 8;
  const childInteract = {
    request: (balance) => {
      const amt = stdlib.le(balance, smallest) ? balance : stdlib.mul(
        stdlib.bigNumberify(Math.floor(Math.random() * UNITS)),
        stdlib.div(balance, stdlib.bigNumberify(UNITS))
      );
      console.log(
        `Child asks for ${stdlib.formatCurrency(amt, 4)}` +
          ` out of ${stdlib.formatCurrency(balance, 4)}`
      );
      return amt;
    },
  };
  const childP = backend.Child(stdlib, childCtc, childInteract);

  await parentP;
  await childP;

  console.log(`\nMulti-sig complete\n`);
  process.exit(0);
})();
