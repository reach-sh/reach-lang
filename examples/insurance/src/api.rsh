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
    init();

    Creator.only(() => {
        const { nftId, minBid, lenInBlocks } = declassify(interact.getSale());
    });
    Creator.publish(nftId, minBid, lenInBlocks);
    const amt = 1;
    commit();
    Creator.pay([[amt, nftId]]);
    Creator.interact.auctionReady();
    assert(balance(nftId) == amt, "balance of NFT is wrong");
    const end = lastConsensusTime() + lenInBlocks;
    const [
        highestBidder,
        lastPrice,
        isFirstBid,
    ] = parallelReduce([Creator, minBid, true])
        .invariant(balance(nftId) == amt && balance() == (isFirstBid ? 0 : lastPrice))
        .while(lastConsensusTime() <= end)
        .api(Bidder.bid,
            ((bid) => { assume(bid > lastPrice, "bid is too low"); }),
            ((bid) => bid),
            ((bid, notify) => {
                require(bid > lastPrice, "bid is too low");
                notify([highestBidder, lastPrice]);
                if (!isFirstBid) {
                    transfer(lastPrice).to(highestBidder);
                }
                const who = this;
                Creator.interact.seeBid(who, bid);
                return [who, bid, false];
            })
        ).timeout(absoluteTime(end), () => {
            Creator.publish();
            return [highestBidder, lastPrice, isFirstBid];
        });

    transfer(amt, nftId).to(highestBidder);
    if (!isFirstBid) { transfer(lastPrice).to(Creator); }
    Creator.interact.showOutcome(highestBidder, lastPrice);
    commit();
    exit();
});