import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const numOfBuyers = 10;

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);

  const accFunder = await stdlib.newTestAccount(startingBalance);
  const accBuyerArray = await Promise.all(
    Array.from({ length: numOfBuyers }, () =>
      stdlib.newTestAccount(startingBalance)
    )
  );

  const ctcFunder = accFunder.deploy(backend);
  const ctcInfo   = ctcFunder.getInfo();

  const funderParams = {
    ticketPrice: stdlib.parseCurrency(5),
    deadline: 5,
  };

  await Promise.all([
    backend.Funder(ctcFunder, {
      showOutcome: (addr) => console.log(`Funder saw ${addr} won.`),
      getParams: () => funderParams,
    }),
  ].concat(
    accBuyerArray.map((accBuyer, i) => {
      const ctcBuyer = accBuyer.attach(backend, ctcInfo);
      return backend.Buyer(ctcBuyer, {
        showOutcome: (outcome) => {
          console.log(`Buyer ${i} saw they ${stdlib.addressEq(outcome, accBuyer) ? 'won' : 'lost'}.`);
        },
        shouldBuyTicket : () => Math.random() < 0.5,
        showPurchase: (addr) => {
          if (stdlib.addressEq(addr, accBuyer)) {
            console.log(`Buyer ${i} bought a ticket.`);
          }
        }
      });
    })
  ));

})();
