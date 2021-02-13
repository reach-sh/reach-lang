import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const numOfPlayers = 10;

(async () => {
  const stdlib = await loadStdlib();
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
      showOutcome: ((addr) =>
        console.log(`Funder saw ${addr} won.`)),
      showWinner: ((ticket) =>
        console.log(`Funder saw ticket #${ticket} won.`)),
      getParams: (() => ({
        ticketPrice: stdlib.parseCurrency(5),
        deadline: 15 })),
    }),
  ].concat(
    await Promise.all(
    accPlayer_arr.map(async (accPlayer, i) => {
      const before = await getBalance(accPlayer);
      const ctcPlayer = accPlayer.attach(backend, ctcInfo);
      let _ticket = undefined;
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
          if ( _ticket === undefined ) {
            _ticket = ticketr;
            console.log(`Player ${i} buying a ticket for ${fmt(ticketPrice)}`);
          }
          return true;
        }),
        buyerWas: (async (addr) => {
          bought = bought || stdlib.addressEq(addr, accPlayer);
        }),
        returnerWas: (async (addr, ticket) => {
          void(addr);
          if ( stdlib.addressEq(addr, accPlayer) ) {
            console.log(`Player ${i} bought ticket #${ticket}`);
          }
        }),
        recoverTicket: (async () => _ticket),
      });
    })
    )));

})();
