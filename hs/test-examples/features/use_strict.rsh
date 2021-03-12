'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      const y = 10;
      A.only(() => {
        'use strict';
        const x = 10;
        const z = 5;
      });
      A.publish(x);

      commit();
      exit();
    });
