'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  init();
  A.publish();
  var [ x ] = [ this ];
  invariant( balance() == 0 );
  while ( true ) {
    commit();
    A.publish();
    enforce(x == this);
    continue;
  }
  commit();
  exit();
});

