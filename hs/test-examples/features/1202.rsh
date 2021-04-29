'reach 0.1';

const AuctionProps = Object({
  startingBid: UInt,
  timeout: UInt });

const BidderProps = {
  getBid: Fun([UInt], Maybe(UInt)) };

const OwnerInterface = {
  showOwner: Fun([Address], Null),
  getAuctionProps: Fun([], AuctionProps),
  ...BidderProps };

const CreatorInterface = {
  ...OwnerInterface,
  getId: Fun([], UInt) };

const maybe = (m, x, f) => fromMaybe(m, (() => x), f);
const fromJust = (m, x) => maybe(m, x, ((y) => y));

export const main = Reach.App(
  {},
  [
    Participant('Creator', CreatorInterface),
    ParticipantClass('Owner', OwnerInterface)
  ],
  (Creator, Owner) => {
    Creator.publish();

    var owner = Creator;
    invariant(balance() == 0);
    while (true) {
      commit();

      Owner.only(() => {
        interact.showOwner(owner);
        const { startingBid, timeout } =
          declassify(interact.getAuctionProps());
      });
      Owner
        .publish(startingBid, timeout)
        .timeout(false);

      const [ timeRemaining, keepGoing ] = makeDeadline(timeout);

      const [ winner, first, currentPrice ] =
        parallelReduce([ owner, true, startingBid ])
          .invariant(balance() == (first ? 0 : currentPrice))
          .while(keepGoing())
          .case(Owner,
            (() => {
              const mbid = (this != owner && this != winner)
                ? declassify(interact.getBid(currentPrice))
                : Maybe(UInt).None();
              return ({
                when: maybe(mbid, false, ((x) => x > currentPrice)),
                msg : fromJust(mbid, 0),
              });
            }),
            ((bid) => bid),
            ((bid) => {
              require(bid > currentPrice);
              if ( ! first ) {
                transfer(currentPrice).to(winner); }
              return [ this, false, bid ];
            })
          )
          .timeRemaining(timeRemaining());

      if ( ! first ) {
        transfer(currentPrice).to(owner); }

      owner = winner;
      continue;
    };

    commit();
    exit();
  });
