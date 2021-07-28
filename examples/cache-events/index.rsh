'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    get: Fun([], UInt),
    spawn: Fun([], Null),
  });
  const B = Participant('B', {

  });
  deploy();

  A.publish();

  var [ i ] = [ 0 ];
  invariant(balance() == 0);
  while ( i < 25 ) {
    commit();

    A.only(() => {
      const a = declassify(interact.get());
    });
    A.publish(a);

    [ i ] = [ i + 1 ];
    continue;
  }

  commit();

  A.interact.spawn();

  B.publish();

  commit();
  exit();

});
