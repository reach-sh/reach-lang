'reach 0.1';

const MUInt = Maybe(UInt);
const common = {
  showOutcome: Fun([Address], Null)
};
const Params = Tuple(Token, UInt, UInt);

export const main = Reach.App(() => {
  const Creator = Participant('Creator', {
    ...common,
    getSale: Fun([], Params),
    seeBid: Fun([Address, UInt], Null),
    timeout: Fun([], Null),
  });
  const Bidder = ParticipantClass('Bidder', {
    ...common,
    seeParams: Fun([Params], Null),
    getBid: Fun([UInt], MUInt),
  });
  deploy();

  Creator.only(() => {
    const [ nftId, reservePrice, lenInBlocks ] = declassify(interact.getSale());
  });
  Creator.publish(nftId, reservePrice, lenInBlocks);
  const amt = 1;
  commit();
  Creator.pay([[amt, nftId]]);
  const end = lastConsensusTime() + lenInBlocks;
  Bidder.interact.seeParams([nftId, reservePrice, end]);

  const [ highestBidder, lastPrice, currentPrice ] =
    parallelReduce([ Creator, 0, reservePrice ])
      .invariant(balance(nftId) == amt && balance() == lastPrice)
      .while(lastConsensusTime() <= end)
      .case(Bidder,
        (() => {
          const mbid = highestBidder != this
            ? declassify(interact.getBid(currentPrice))
            : MUInt.None();
          return ({
            when: maybe(mbid, false, ((b) => b > currentPrice)),
            msg : fromSome(mbid, 0)
          });
        }),
        ((bid) => bid),
        ((bid) => {
          require(bid > currentPrice);
          transfer(lastPrice).to(highestBidder);
          Creator.interact.seeBid(this, bid);
          return [ this, bid, bid ];
        }))
      .timeout(absoluteTime(end), () => {
        Creator.interact.timeout();
        Creator.publish();
        return [ highestBidder, lastPrice, currentPrice ];
      });
  transfer(lastPrice).to(Creator);
  transfer(amt, nftId).to(highestBidder);
  commit();

  each([Creator, Bidder], () => interact.showOutcome(highestBidder));
  exit();
});
