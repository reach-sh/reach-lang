import { loadStdlib } from "@reach-sh/stdlib";
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib({REACH_NO_WARN: 'Y'});
const MAX = 50;
const accA = await stdlib.newTestAccount(stdlib.parseCurrency(5000));
const ctcA = accA.contract(backend);
const tickets = await stdlib.launchToken(accA, "Tickets", "TIX", {supply: MAX});

console.log('Welcome to the ticket distributor\nLets get you a ticket');

const startBuyers = async () => {
  await stdlib.wait(10);// need this to work on ETH
  const runBuyers = async (i) => {
    const acc = await stdlib.newTestAccount(stdlib.parseCurrency(100));
    const ctc = acc.contract(backend, ctcA.getInfo());
    await acc.tokenAccept(tickets.id);
    await ctc.apis.Buyer.buyTicket();
    console.log(`Tickets sold: ${i}`);
  }// end of runBuyers
  for(let i = 0; i < MAX; i++){
    await runBuyers(i);
  }
}// end of startBuyers

await Promise.all([
  backend.Admin(ctcA, {
    params: {
      tok: tickets.id,
      cost: stdlib.parseCurrency(10),
      supply: MAX,
    },
    launched: (contract) => {
      console.log(`Ready at contract: ${contract}`);
      startBuyers();
    },
  }),
]);
console.log('Exiting...');
