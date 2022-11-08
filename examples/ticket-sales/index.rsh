'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Admin', {
    params: Object({
      cost: UInt,
      tok: Token,
      supply: UInt,
    }),
    launched: Fun([Contract], Null),
  });
  const B = API('Buyer', {
    buyTicket: Fun([], Null),
  });
  init();

  A.only(() => {
    const {cost, tok, supply} = declassify(interact.params);
  });
  A.publish(cost, tok, supply);
  commit();
  A.pay([[supply, tok]]);
  A.interact.launched(getContract());

  const [ticketsSold] = parallelReduce([0])
    .invariant(balance() == cost * ticketsSold)
    .invariant(balance(tok) == supply - ticketsSold)
    .while(ticketsSold < supply)
    .api_(B.buyTicket, () => {
      check(ticketsSold != supply, "sorry, out of tickets");
      return[cost, (ret) => {
        transfer(1, tok).to(this);
        ret(null);
        return [ticketsSold + 1];
      }];
    });
  transfer(cost * ticketsSold).to(A);
  commit();
  exit();
});

