'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  init();
  A.publish();

  var [] = [];
  invariant( balance() == 0 );
  while ( true ) {
    var [] = [];
    invariant( balance() == 0 );
    while ( false ) {
      commit();
      A.publish();
      continue;
    }
    continue;
  }
  commit();
});

