'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      const y = 10;
      A.only(() => {
        'use strict'; // test that 'use strict' is scope sensitive!
        const x = 10;
      });
      A.publish(x);

      commit();
      exit();
    });
