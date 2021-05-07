'reach 0.1';
'use strict';

// FOMO Workshop generalized to last N winners
const NUM_OF_WINNERS = 3;

const CommonInterface = {
  showOutcome: Fun([Array(Address, NUM_OF_WINNERS)], Null),
};

const FunderInterface = {
  ...CommonInterface,
  getParams: Fun([], Object({
    deadline: UInt, // relative deadline
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
    Participant('Funder', FunderInterface), ParticipantClass('Buyer', BuyerInterface),
  ],
  (Funder, Buyer) => {
    const showOutcome = (winners) =>
      each([Funder, Buyer], () => interact.showOutcome(winners));

    Funder.only(() => {
      const { ticketPrice, deadline } = declassify(interact.getParams()); });
    Funder.publish(ticketPrice, deadline);

    const initialWinners = Array.replicate(NUM_OF_WINNERS, Funder);

    // Until deadline, allow buyers to buy ticket
    const [ keepGoing, winners, ticketsSold ] =
      parallelReduce([ true, initialWinners, 0 ])
        .invariant(balance() == ticketsSold * ticketPrice)
        .while(keepGoing)
        .case(
          Buyer,
          (() => ({
            when: declassify(interact.shouldBuyTicket(ticketPrice)) })),
          ((_) => ticketPrice),
          ((_) => {
            const buyer = this;
            Buyer.only(() => interact.showPurchase(buyer));
            const idx = ticketsSold % NUM_OF_WINNERS;
            const newWinners =
              Array.set(winners, idx, buyer);
            return [ true, newWinners, ticketsSold + 1 ]; }))
        .timeout(deadline, () => {
          Anybody.publish();
          return [ false, winners, ticketsSold ]; });

    transfer(balance() % NUM_OF_WINNERS).to(Funder);
    const reward = balance() / NUM_OF_WINNERS;

    winners.forEach(winner =>
      transfer(reward).to(winner));

    commit();
    showOutcome(winners);
  });
