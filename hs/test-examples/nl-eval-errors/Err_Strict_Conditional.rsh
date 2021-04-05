'reach 0.1';
'use strict';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      const x = 1 ? 2 : 3;
      void x;
    });
