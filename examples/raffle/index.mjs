import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const numOfPlayers = 2;

  const stdlib = loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);

  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const getBalance = async (who) => fmt(await stdlib.balanceOf(who));

  const [ accSponsor, ...accPlayer_arr ] =
    await stdlib.newTestAccounts(numOfPlayers+1, startingBalance);
  accSponsor.setDebugLabel('Sponsor');
  accPlayer_arr.forEach((acc, idx) => acc.setDebugLabel(`Player${idx}`));

  const ctcSponsor = accSponsor.contract(backend);
  const ctcInfo = ctcSponsor.getInfo();

  await Promise.all([
    backend.Sponsor(ctcSponsor, {
      ...stdlib.hasRandom,
      getParams: (() => ({
        ticketPrice: stdlib.parseCurrency(5),
        deadline: numOfPlayers * (stdlib.connector === 'ALGO' ? 5 : 75) })),
      showOpen: (() =>
        console.log(`Sponsor saw ticket sales open`)),
      showReturning: ((howMany) =>
        console.log(`Sponsor saw ${howMany} tickets sold`)),
      showReturned: ((howManyReturned) =>
        console.log(`Sponsor saw ${howManyReturned} tickets revealed`)),
      showWinner: ((ticket) =>
        console.log(`Sponsor saw ticket #${ticket} won.`)),
      showOutcome: ((addr) =>
        console.log(`Sponsor saw ${stdlib.formatAddress(addr)} won.`)),
    }),
  ].concat(
    await Promise.all(
    accPlayer_arr.map(async (accPlayer, i) => {
      const before = await getBalance(accPlayer);
      const ctcPlayer = accPlayer.contract(backend, ctcInfo);
      let bought = false;
      return backend.Player(ctcPlayer, {
        ...stdlib.hasRandom,
        showOutcome: (async (addr) => {
          const after = await getBalance(accPlayer);
          console.log(`Player${i} saw they ${stdlib.addressEq(addr, accPlayer) ? 'won' : 'lost'}; balance: ${before} => ${after}`);
        }),
        showWinner: (async (ticket) =>
          void(ticket)),
        shouldBuy: (async (ticketPrice, ticketr) => {
          if ( bought ) { return false; }
          console.log(`Player${i} trying to buy a ticket for ${fmt(ticketPrice)}`);
          return true;
        }),
        didBuy: (async () => {
          console.log(`Player${i} bought a ticket`);
          bought = true;
        }),
        didReturn: (async (ticket) => {
          console.log(`Player${i} returned and revealed ticket #${ticket}`);
        }),
      });
    })
    )));
