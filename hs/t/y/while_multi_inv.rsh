'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: Fun([], UInt)
  });
  init();

  A.publish();

  var [ x ] = [ 0 ];
  invariant(x < 10, "x < 10");
  invariant(balance() == 0, "balance is zero");
  while ( x < 15 ) {
    commit();

    A.only(() => {
      const ax = declassify(interact.x());
    });
    A.publish(ax).check(() => { check(ax < 10); });

    [ x ] = [ ax ];
    continue;
  }
  commit();
  check(x < 10);
})
