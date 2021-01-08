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
    ticketPrice: stdlib.parseCurrency(3),
    deadline: 8,
  };

  const resultText = (outcome, addr) =>
    outcome.includes(addr) ? 'won' : 'lost';

  await Promise.all([
    backend.Funder(ctcFunder, {
      showOutcome: (outcome) =>
        console.log(`Funder saw they ${resultText(outcome, accFunder.networkAccount.address)}`),
      getParams: () => funderParams,
    }),
  ].concat(
    accBuyerArray.map((accBuyer, i) => {
      const ctcBuyer = accBuyer.attach(backend, ctcInfo);
      const Who = `Buyer #${i}`;
      return backend.Buyer(ctcBuyer, {
        showOutcome: (outcome) =>
          console.log(`${Who} saw they ${resultText(outcome, accBuyer.networkAccount.address)}`),
        shouldBuyTicket : () => Math.random() < 0.5,
        showPurchase: (addr) => {
          if (stdlib.addressEq(addr, accBuyer)) {
            console.log(`${Who} bought a ticket.`);
          }
        }
      });
    })
  ));
})();
