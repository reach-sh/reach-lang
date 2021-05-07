'reach 0.1';
'use strict';

export const main = Reach.App(
  {},
  [ Participant('A', {}) ],
  (A) => {
    A.publish();

    var [ h1, h2 ] = [ 21, 21 ];
    { const sum = () => h1 + h2; }
    invariant(sum() >= 0 && balance() == 0);
    while ( sum() > 0 ) {
      commit();
      A.publish();
      [ h1, h2 ] = [ h1 - 1, sum() / 2 ];
      continue;
    }
    commit();

    A.pay(sum());
    commit();

    exit();
  });

