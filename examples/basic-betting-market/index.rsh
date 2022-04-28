'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const O = Participant('Organizer', {
    contract: Contract,
    bettingTime: UInt,
    payoutTime: UInt,
    bookieCut: FixedPoint, // 0-1 coefficient of losers' bets to take as cut
    ready: Fun([], Null),
  });
  const OA = API('OrganizerAPI', {
    startBets: Fun([], Null),
    bettingTimeout: Fun([], Null),
    startPayout: Fun([], Null),
    payoutTimeout: Fun([], Null),
  });
  const B = API('Better', {
    bet: Fun([Bool, UInt], Null),
    collect: Fun([], Null),
  });
  const Phase = Data({ AcceptingBets: Null, AwaitingResult: Null, Payout: Null });
  const BP = Events('BettingPhase', { phase: [Phase] });
  init();

  const uint2fx = (n) => fx(1)(Pos, n);
  const fx2uint = (n) => fxrescale(n, 1).i.i; // truncating! lossy!
  const zero = uint2fx(0);
  const one = uint2fx(1);

  const awaitOrganizer = (api) => {
    const [[], k] = call(api).assume(() => check(this == O));
    check(this == O);
    k(null);
  }

  // Set up the book
  O.only(() => {
    const contract = declassify(interact.contract);
    const bettingTime = declassify(interact.bettingTime);
    const payoutTime = declassify(interact.payoutTime);
    const bookieCut = declassify(interact.bookieCut);
    check(fxgt(bookieCut, zero) && fxlt(bookieCut, one));
  });
  O.publish(contract, bettingTime, payoutTime, bookieCut);
  check(fxgt(bookieCut, zero) && fxlt(bookieCut, one));
  commit();

  O.interact.ready();
  awaitOrganizer(OA.startBets);
  BP.phase(Phase.AcceptingBets());

  const trueBets = new Map(UInt);
  const falseBets = new Map(UInt);
  const checkValidBet = (better, guess, amt) => {
    check(better != O);
    check(amt > 0);
    if (guess) {
      check(isNone(falseBets[better]));
    } else {
      check(isNone(trueBets[better]));
    }
  };

  // Take bets
  const [keepGoing, trueBetters, truePool, falseBetters, falsePool] =
    parallelReduce([true, 0, 0, 0, 0])
    .while(keepGoing)
    .invariant(trueBets.sum() + falseBets.sum() == balance()
               && truePool == trueBets.sum()
               && falsePool == falseBets.sum()
               && truePool + falsePool == balance()
               && trueBetters == trueBets.size()
               && falseBetters == falseBets.size()
               && trueBets.all(x => x > 0)
               && falseBets.all(x => x > 0))
    .api(B.bet,
      (guess, amt) => checkValidBet(this, guess, amt),
      (_, amt) => amt,
      (guess, amt, k) => {
        checkValidBet(this, guess, amt);
        k(null);
        if (guess) {
          const newBetter = isNone(trueBets[this]) ? 1 : 0;
          trueBets[this] = fromSome(trueBets[this], 0) + amt;
          return [true, trueBetters + newBetter, truePool + amt, falseBetters, falsePool];
        } else {
          const newBetter = isNone(falseBets[this]) ? 1 : 0;
          falseBets[this] = fromSome(falseBets[this], 0) + amt;
          return [true, trueBetters, truePool, falseBetters + newBetter, falsePool + amt];
        }
      }
    )
    .timeout(relativeTime(bettingTime), () => {
      awaitOrganizer(OA.bettingTimeout);
      return [false, trueBetters, truePool, falseBetters, falsePool];
    });

  BP.phase(Phase.AwaitingResult());
  commit();

  assert(truePool + falsePool == balance());

  // Await result
  // const result = true;

  // Pay winners
  const numWinners = /* result ? */ trueBetters /* : falseBetters */;
  const winnerBets = /* result ? */ trueBets /* : falseBets */;
  const winnerPool = /* result ? */ truePool /* : falsePool */;
  const loserPool = /* result ? */ falsePool /* : truePool */;

  // Do truncation in favor of winners, against bookie
  const organizerCut = 0; //fx2uint(fxmul(bookieCut, uint2fx(loserPool)));
  const initialPayoutPool = loserPool - organizerCut;

  const checkWinner = p => {
    const bet = winnerBets[p];
    check(bet != Maybe(UInt).None());
    check(bet != Maybe(UInt).Some(0));
  }
  const unwrap = m => {
    switch (m) {
      case Some: return m;
      case None:
        assert(false, "unwrap None");
        return 0;
    }
  }

  awaitOrganizer(OA.startPayout);
  const [keepGoing_, unpaidWinners, payoutPool] =
    parallelReduce([true, numWinners, initialPayoutPool])
    .while(keepGoing_ && unpaidWinners > 0)
    .invariant(winnerBets.size() == unpaidWinners
               && balance() == winnerBets.sum() + payoutPool + organizerCut)
    .api(B.collect,
      () => checkWinner(this),
      () => 0,
      (k) => {
        checkWinner(this);
        k(null);
        const mBet = winnerBets[this];
        assert(mBet != Maybe(UInt).Some(0), "zero bet winner");
        assert(mBet != Maybe(UInt).None(), "non better winner");
        assert(isSome(mBet), "non better winner 3");
        switch (mBet) {
          case None: assert(false, "non better winner 2");
          default: ;
        }

        const bet = unwrap(mBet);
        const share = fxdiv(uint2fx(bet), uint2fx(winnerPool), 100_000);
        const winnings = fx2uint(fxmul(share, uint2fx(initialPayoutPool)));
        delete winnerBets[this];
        transfer(bet + winnings).to(this);
        return [true, unpaidWinners - 1, payoutPool - winnings];
      }
    )
    .timeout(relativeTime(payoutTime), () => {
      awaitOrganizer(OA.payoutTimeout);
      return [false, unpaidWinners, payoutPool];
    });

  transfer(balance()).to(O);
  commit();
  exit();
});
