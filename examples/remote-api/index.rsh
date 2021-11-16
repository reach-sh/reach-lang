'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    ...hasConsoleLogger,
    initCall: Fun(true, Null),
    ctcInfo: Fun([], Contract),
    getGuess: Fun([], UInt),
  });

  const C = API({
    incr: Fun([UInt], UInt)
  });

  const V = View({ isDone: Bool });

  deploy();

  A.publish();
  V.isDone.set(false);

  var [ i ] = [ 0 ];
  invariant(balance() == 0);
  while (i < 5) {
    commit();

    A.interact.initCall();

    const [ [ci], k ] =
      call(C.incr)
        .assume((j) =>
          assume(j + i < UInt.max, "Does not cause overflow"));

    A.interact.log(ci);
    const i_ = i + ci;
    k(i_);
    [ i ] = [ i_ ];
    continue;
  }
  commit();

  A.only(() => {
    const ctcInfo = declassify(interact.ctcInfo());
    const g = declassify(interact.getGuess());
  });
  A.publish(ctcInfo, g);

  // Communicate with another Reach ctc
  const r = remote(ctcInfo, {
    guess: Fun([UInt], Bool),
  });

  const isCorrect = r.guess(g);
  A.interact.log("Guess correct: ", isCorrect);

  V.isDone.set(true);
  commit();

  exit();
});
