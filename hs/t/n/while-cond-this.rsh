'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  init();
  A.publish();
  var [] = [];
  invariant( balance() == 0 );
  while ( this == A ) {
    commit();
    A.publish();
    continue;
  }
  commit();
  exit();
});

