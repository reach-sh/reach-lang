import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const role = 'seller';
console.log(`Your role is ${role}`);

const stdlib = loadStdlib(process.env);
console.log(`The consensus network is ${stdlib.connector}.`);

const commonInteract = {};

// Seller
if (role === 'seller') {
  const sellerInteract = {
    ...commonInteract,
  };

  const acc = await stdlib.newTestAccount(stdlib.parseCurrency(1000));
  const ctc = acc.contract(backend);
  await ctc.participants.Seller(sellerInteract);

// Buyer
} else {
  const buyerInteract = {
    ...commonInteract,
  };

};
