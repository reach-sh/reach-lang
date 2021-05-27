'reach 0.1';
'use strict';

const CommonInterface = {
  // Show the address of winner
  showOutcome: Fun([Address], Null),
};

const FunderInterface = {
  ...CommonInterface,
  getParams: Fun([], Object({
    // relative deadline
    deadline: UInt,
    ticketPrice: UInt,
  })),
};

const BuyerInterface = {
  ...CommonInterface,
  shouldBuyTicket: Fun([UInt], Bool),
  showPurchase: Fun([Address], Null),
};

export const main = Reach.App(
  { },
  [
    Participant('Funder', FunderInterface),
    ParticipantClass('Buyer', BuyerInterface),
  ],
  (Funder, Buyer) => {

    // Helper to display results to everyone
    const showOutcome = (who) =>
      each([Funder, Buyer], () => {
        interact.showOutcome(who); });

    // Have the funder publish the ticket price and deadline
    Funder.only(() => {
      const { ticketPrice, deadline } =
        declassify(interact.getParams());
    });
    Funder.publish(ticketPrice, deadline);

    // Until timeout, allow buyers to buy ticket
    const [ keepGoing, winner, ticketsSold ] =
      parallelReduce([ true, Funder, 0 ])
        .invariant(balance() == ticketsSold * ticketPrice)
        .while(keepGoing)
        .case(
          Buyer,
          (() => ({
            when: declassify(interact.shouldBuyTicket(ticketPrice)),
          })),
          ((_) => ticketPrice),
          ((_) => {
            const buyer = this;
            Buyer.only(() => interact.showPurchase(buyer));
            return [ true, buyer, ticketsSold + 1 ];
          })
        )
        .timeout(deadline, () => {
          Anybody.publish();
          return [ false, winner, ticketsSold ]; });

    // Whoever buys last wins and receives balance
    transfer(balance()).to(winner);
    commit();
    showOutcome(winner);
  });
