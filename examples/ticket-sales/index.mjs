import { loadStdlib } from "@reach-sh/stdlib";
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib({REACH_NO_WARN: 'Y'});
const MAX = 50;
const accA = await stdlib.newTestAccount(stdlib.parseCurrency(5000));
const ctcA = accA.contract(backend);
const tickets = await stdlib.launchToken(accA, "Tickets", "TIX", {supply: MAX});

console.log('Welcome to the ticket distributor\nLets get you a ticket');

const startBuyers = async () => {
  const runBuyer = async (i) => {
    const acc = await stdlib.newTestAccount(stdlib.parseCurrency(100));
    const ctc = acc.contract(backend, ctcA.getInfo());
    await acc.tokenAccept(tickets.id);
    await ctc.apis.Buyer.buyTicket();
    console.log(`Tickets sold: ${i}`);
  }// end of runBuyer
  for(let i = 0; i < MAX; i++){
    await runBuyer(i);
  }
}// end of startBuyers

await ctcA.p.Admin({
  params: {
    tok: tickets.id,
    cost: stdlib.parseCurrency(10),
    supply: MAX,
  },
  launched: async (contract) => {
    console.log(`Ready at contract: ${contract}`);
    await startBuyers();
  },
}),
console.log('Exiting...');

