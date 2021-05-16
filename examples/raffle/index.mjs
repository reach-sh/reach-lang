import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const numOfPlayers = 5;

(async () => {
  const stdlib = await loadStdlib();
  if ( stdlib.connector === 'ALGO' ) {
    console.log(`XXX Unsupported`);
    process.exit(0);
  }
  const startingBalance = stdlib.parseCurrency(100);

  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const getBalance = async (who) => fmt(await stdlib.balanceOf(who));

  const accSponsor = await stdlib.newTestAccount(startingBalance);
  const accPlayer_arr = await Promise.all(
    Array.from({ length: numOfPlayers }, () =>
      stdlib.newTestAccount(startingBalance)));

  const ctcSponsor = accSponsor.deploy(backend);
  const ctcInfo = ctcSponsor.getInfo();

  await Promise.all([
    backend.Sponsor(ctcSponsor, {
      ...stdlib.hasRandom,
      getParams: (() => ({
        ticketPrice: stdlib.parseCurrency(5),
        deadline: 50 })),
      showOpen: (() =>
        console.log(`Sponsor saw ticket sales open`)),
      showReturning: ((howMany) =>
        console.log(`Sponsor saw ${howMany} tickets sold`)),
      showReturned: ((howManyReturned) =>
        console.log(`Sponsor saw ${howManyReturned} tickets revealed`)),
      showWinner: ((ticket) =>
        console.log(`Sponsor saw ticket #${ticket} won.`)),
      showOutcome: ((addr) =>
        console.log(`Sponsor saw ${addr} won.`)),
    }),
  ].concat(
    await Promise.all(
    accPlayer_arr.map(async (accPlayer, i) => {
      const before = await getBalance(accPlayer);
      const ctcPlayer = accPlayer.attach(backend, ctcInfo);
      let bought = false;
      return backend.Player(ctcPlayer, {
        ...stdlib.hasRandom,
        showOutcome: (async (addr) => {
          const after = await getBalance(accPlayer);
          console.log(`Player ${i} saw they ${stdlib.addressEq(addr, accPlayer) ? 'won' : 'lost'}; balance: ${before} => ${after}`);
        }),
        showWinner: (async (ticket) =>
          void(ticket)),
        shouldBuy: (async (ticketPrice, ticketr) => {
          if ( bought ) { return false; }
          console.log(`Player ${i} trying to buy a ticket for ${fmt(ticketPrice)}`);
          return true;
        }),
        buyerWas: (async (addr) => {
          const bought_n = bought || stdlib.addressEq(addr, accPlayer);
          if ( bought == false && bought_n == true ) {
            console.log(`Player ${i} bought a ticket`);
          }
          bought = bought_n;
        }),
        returnerWas: (async (addr, ticket) => {
          void(addr);
          if ( stdlib.addressEq(addr, accPlayer) ) {
            console.log(`Player ${i} returned and revealed ticket #${ticket}`);
          }
        }),
      });
    })
    )));

})();
