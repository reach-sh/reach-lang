import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const numOfBidders = 5;

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(10);

  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const getBalance = async (who) => fmt(await stdlib.balanceOf(who));

  const accSponsor = await stdlib.newTestAccount(startingBalance);
  const accBidder_arr = await Promise.all(
    Array.from({ length: numOfBidders }, () =>
      stdlib.newTestAccount(startingBalance)));

  const beforeSponsor = await getBalance(accSponsor);
  const ctcSponsor = accSponsor.deploy(backend);
  const ctcInfo = ctcSponsor.getInfo();

  await Promise.all([
    backend.Sponsor(ctcSponsor, {
      getParams: (() => ({
        prize: stdlib.parseCurrency(5),
        deadline: numOfBidders * 2 })),
      showWinner: (async (done, winner, winningBid) => {
        console.log(`Sponsor saw ${winner} won @ ${fmt(winningBid)}`);
        if ( done ) {
          const after = await getBalance(accSponsor);
          console.log(`Sponsor balance: ${beforeSponsor} => ${after}`);
        }
      }),
    }),
  ].concat(
    await Promise.all(
    accBidder_arr.map(async (accBidder, i) => {
      const before = await getBalance(accBidder);
      const ctcBidder = accBidder.attach(backend, ctcInfo);
      let value = undefined;
      return backend.Bidder(ctcBidder, {
        showWinner: (async (done, winner, winningBid) => {
          console.log(`Bidder ${i} saw ${winner} won @ ${fmt(winningBid)}`);
          if ( done ) {
            const after = await getBalance(accBidder);
            console.log(`Bidder ${i} saw they ${stdlib.addressEq(winner, accBidder) ? 'won' : 'lost'}; balance: ${before} => ${after}`);
          }
        }),
        getBid: ((prize, winningBid, previousBid) => {
          if ( value === undefined ) {
            const valuep = 90 + Math.floor(Math.random() * 10);
            value = prize.mul(valuep).div(100);
          }
          if ( winningBid.lt(value) ) {
            const totSurplus = value.sub(winningBid);
            const more = totSurplus.div(10);
            const totBid = winningBid.add(more);
            const addl = totBid.sub(previousBid);
            console.log(`Bidder ${i} bids ${fmt(previousBid)} + ${fmt(addl)}`);
            return addl;
          } else {
            return 0;
          }
        }),
      });
    })
    )));
})();
