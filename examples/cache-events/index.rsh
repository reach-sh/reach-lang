'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    get: Fun([], UInt),
  });
  const B = Participant('B', {

  });
  deploy();

  A.publish();

  var [ i ] = [ 0 ];
  invariant(balance() == 0);
  while ( i < 20 ) {
    commit();

    A.only(() => {
      const a = declassify(interact.get());
    });
    A.publish(a);

    [ i ] = [ i + 1 ];
    continue;
  }

  commit();

  B.publish();

  commit();
  exit();

});
