import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);

const accs = await stdlib.newTestAccounts(4, startingBalance);
const [accA, acc1, acc2, acc3] = accs;
const ctcA = accA.contract(backend);

const startUp = async () => {
  const flag = "startup incomplete";
  try{
    await ctcA.p.Admin({
      max: 4,
      launched: (c) => {
        throw flag;
      },
    });
  } catch (e) {
    if(e !== flag) throw e;
  };
};

const countMe = async (addr) => {
  const num = await ctc(addr).a.countUp();
  console.log(`Your number is ${num}`);
};

const ctcinfo = ctcA.getInfo();
const ctc = (acc) => acc.contract(backend, ctcinfo);

await startUp();
console.log(`Startup successful`);

await countMe(acc1);
await countMe(acc2);
await countMe(acc3);

console.log(`Finished testing.`);

