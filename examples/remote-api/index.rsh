'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    ...hasConsoleLogger,
    initCall: Fun(true, Null),
  });
  const C = API({
    incr: Fun([UInt], UInt)
  });
  deploy();
  A.publish();

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
  exit();
});
