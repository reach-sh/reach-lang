'reach 0.1';

export const main = Reach.App(() => {
    const Creator = Participant('Creator', {
        getSale: Fun([], Object({
            nftId: Token,
            minBid: UInt,
            lenInBlocks: UInt,
        })),
        auctionReady: Fun([], Null),
        seeBid: Fun([Address, UInt], Null),
        showOutcome: Fun([Address, UInt], Null),
    });
    const Bidder = API('Bidder', {
        bid: Fun([UInt], Tuple(Address, UInt)),
    });
    const V = View({
      min: UInt,
      nft: Token,
      currentBid: UInt,
    })
    init();

    Creator.only(() => {
        const {nftId, minBid, lenInBlocks} = declassify(interact.getSale());
    });
    Creator.publish(nftId, minBid, lenInBlocks);
    const amt = 1;
    commit();
    Creator.pay([[amt, nftId]]);
    Creator.interact.auctionReady();
    assert(balance(nftId) == amt, "balance of NFT is wrong");
    V.min.set(minBid);
    V.nft.set(nftId);
    const end = lastConsensusTime() + lenInBlocks;
    const [
        highestBidder,
        lastPrice,
        isFirstBid,
    ] = parallelReduce([Creator, minBid, true])
        .define(() => {
          V.currentBid.set(lastPrice);
        })
        .invariant(balance(nftId) == amt)
        .invariant(balance() == (isFirstBid ? 0 : lastPrice))
        .while(lastConsensusTime() <= end)
        .api_(Bidder.bid, (bid) => {
            check(bid > lastPrice, "bid is too low");
            return [ bid, (notify) => {
                notify([highestBidder, lastPrice]);
                if ( ! isFirstBid ) {
                    transfer(lastPrice).to(highestBidder);
                }
                const who = this;
                Creator.interact.seeBid(who, bid);
                return [who, bid, false];
            }];
        })
        .timeout(absoluteTime(end), () => {
            Creator.publish();
            return [highestBidder, lastPrice, isFirstBid];
        });

        transfer(amt, nftId).to(highestBidder);
        if ( ! isFirstBid ) { transfer(lastPrice).to(Creator); }
        Creator.interact.showOutcome(highestBidder, lastPrice);
    commit();
    exit();
});

