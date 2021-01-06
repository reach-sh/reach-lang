'reach 0.1';

// FOMO Workshop generalized to last N winners
const NUM_OF_WINNERS = 3;

const MaybeAddr = Maybe(Address);

const CommonInterface = {
  showOutcome: Fun([Array(MaybeAddr, NUM_OF_WINNERS)], Null),
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
  shouldBuyTicket: Fun([], Bool),
  showPurchase: Fun([Address], Null),
};

export const main = Reach.App(
  { },
  [
    ['Funder', FunderInterface], ['class', 'Buyer', BuyerInterface],
  ],
  (Funder, Buyer) => {
    const showOutcome = (winners) =>
      each([Funder, Buyer], () => interact.showOutcome(winners));

    Funder.only(() => {
      const { ticketPrice, deadline } = declassify(interact.getParams()); });
    Funder.publish(ticketPrice, deadline);

    const initialWinners = Array.replicate(NUM_OF_WINNERS, MaybeAddr.None());

    // Until deadline, allow buyers to buy ticket
    const [ keepGoing, winners, ticketsSold ] =
      parallel_reduce([ true, initialWinners, 0 ])
        .invariant(balance() == ticketsSold * ticketPrice)
        .while(keepGoing)
        .case(
          Buyer,
          (() => ({
            when: declassify(interact.shouldBuyTicket()) })),
          (() => ticketPrice),
          () => {
            Buyer.only(() => interact.showPurchase(this));
            const idx = ticketsSold % NUM_OF_WINNERS;
            const newWinners =
              Array.set(winners, idx, MaybeAddr.Some(this));
            return [ true, newWinners, ticketsSold + 1 ]; })
        .timeout(deadline, () => {
          race(Buyer, Funder).publish();
          return [ false, winners, ticketsSold ]; });

    const howManyBuyers = winners.count(isSome);

    if (howManyBuyers == 0) {
      transfer(balance()).to(Funder);
    } else {
      // If there are 3 winners, ticket price = $4, 10 bids = $40.
      // 40 /= 3. Give 1 to funder, split 39 between 3 winners
      transfer(balance() % howManyBuyers).to(Funder);
      const reward = balance() / howManyBuyers;

      winners.forEach(winner =>
        fromMaybe(winner, () => {}, w => transfer(reward).to(w))); }

    commit();
    showOutcome(winners);
  });
