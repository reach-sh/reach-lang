'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const O = Participant('Organizer', {
    contract: Contract,
    bettingTime: UInt,
    payoutTime: UInt,
    bookieCut: UInt, // 0-1000 tenths of a percent to keep as a cut
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
    check(bookieCut <= 1000);
  });
  O.publish(contract, bettingTime, payoutTime, bookieCut);
  check(bookieCut <= 1000);
  commit();

  O.interact.ready();
  awaitOrganizer(OA.startBets);
  BP.phase(Phase.AcceptingBets());

  // Take bets
  const bets = new Map(Tuple(Bool, UInt));
  const numBettingOn = (guess) => bets.reduce(0, (s, [g, _]) => s + (g == guess ? 1 : 0));
  const amountBetOn  = (guess) => bets.reduce(0, (s, [g, a]) => s + (g == guess ? a : 0));
  const [keepGoing, trueBetters, truePool, falseBetters, falsePool] =
    parallelReduce([false, 0, 0, 0, 0])
    .while(keepGoing)
    .invariant(trueBetters + falseBetters == bets.size()
               && numBettingOn(true) == trueBetters
               && numBettingOn(false) == falseBetters
               && numBettingOn(true) + numBettingOn(false) == bets.size()
               && truePool + falsePool == bets.reduce(0, (s, [_, a]) => s + a)
               && truePool + falsePool == balance()
               && amountBetOn(true) == truePool
               && amountBetOn(false) == falsePool
               && amountBetOn(true) + amountBetOn(false) == balance())
    .api(B.bet,
      (_, amt) => check(isNone(bets[this]) && amt > 0),
      (_, amt) => amt,
      (guess, amt, k) => {
        check(isNone(bets[this]) && amt > 0);
        k(null);
        bets[this] = [guess, amt];
        if (guess) {
          return [true, trueBetters + 1, truePool + amt, falseBetters, falsePool];
        } else {
          return [true, trueBetters, truePool, falseBetters + 1, falsePool + amt];
        }
      }
    )
    .timeout(relativeTime(bettingTime), () => {
      awaitOrganizer(OA.bettingTimeout);
      return [false, trueBetters, truePool, falseBetters, falsePool];
    });

  assert(truePool + falsePool == balance());

  // Await result
  BP.phase(Phase.AwaitingResult());
  commit();
  const result = true;

  // Pay winners
  awaitOrganizer(OA.startPayout);

  const unwrap = m => {
    switch (m) {
      case Some:
        return m;
      case None:
        assert(false, "unwrap on None");
        return [false, 0];
    }
  }
  const checkWinner = p => {
    const bet = bets[p];
    switch (bet) {
      case Some:
        check(bet[0] == result);
        check(bet[1] > 0);
      case None:
        check(false);
    }
  }

  const [numWinners, winnerPool, loserPool] = /*result ?*/ [trueBetters, truePool, falsePool]
                                                     /*: [falseBetters, falsePool, truePool]*/;
  const finalBookieCut = muldiv(bookieCut, loserPool, 1000);
  const initWinningsPool = loserPool - finalBookieCut;

  assert(truePool + falsePool == balance());
  assert(winnerPool + loserPool == balance());

  const [keepGoing_, unpaidWinners, winningsPool] =
    parallelReduce([true, numWinners, initWinningsPool])
    .while(keepGoing_ && unpaidWinners > 0)
    .invariant(numBettingOn(result) == unpaidWinners &&
               balance() == finalBookieCut + amountBetOn(result) + winningsPool
              )
    .api(B.collect,
      () => checkWinner(this),
      () => 0,
      (k) => {
        checkWinner(this);
        k(null);
        const bet = unwrap(bets[this])[1];
        assert(bet <= winnerPool); // since amountBetOn(result) == winnerPool, this is always true, but it fails

        const winnings = muldiv(loserPool, bet, winnerPool);
        const payout = bet + winnings;
        transfer(payout).to(this);
        delete bets[this];
        return [true, unpaidWinners - 1, winningsPool - winnings];
      }
    )
    .timeout(relativeTime(payoutTime), () => {
      awaitOrganizer(OA.payoutTimeout);
      return [false, unpaidWinners, winningsPool];
    });

  transfer(balance()).to(O);
  commit();
  exit();
});
