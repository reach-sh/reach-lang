'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  init();
  A.publish();
  var [] = [];
  { const cool = this; }
  invariant( balance() == 0 );
  while ( true ) {
    commit();
    A.publish();
    continue;
  }
  enforce( this == A );
  commit();
  exit();
});

