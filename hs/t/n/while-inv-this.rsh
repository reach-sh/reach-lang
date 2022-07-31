'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  init();
  A.publish();
  var [] = [];
  invariant( this == A );
  while ( true ) {
    commit();
    A.publish();
    continue;
  }
  commit();
  exit();
});

