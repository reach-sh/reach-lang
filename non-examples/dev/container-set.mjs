import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/container-set.main.mjs';

const main = async () => {
  const stdlib = await loadStdlib();
  const { newTestAccount, parseCurrency } = stdlib;

  const startingBalance = parseCurrency(100);

  const accAlice = await newTestAccount(startingBalance);
  const accBob   = await newTestAccount(startingBalance);

  const ctcAlice = accAlice.deploy(backend);
  const ctcBob   = accBob.attach(backend, ctcAlice.getInfo());

  const accounts = {
    'Alice': accAlice,
    'Bob'  : accBob,
  };

  const interactWith = (name) => ({
    getAddr: async () => {
      console.log(`Returning address for ${name}.`);
      return await accounts[name].networkAccount.getAddress();
    },
    showResult: (b) => {
      console.log(`${name} saw outcome: ${b}`);
    },
  });

  await Promise.all([
    backend.Alice(stdlib, ctcAlice, interactWith('Alice')),
    backend.Bob(stdlib, ctcBob, interactWith('Bob')),
  ]);
};

main();
