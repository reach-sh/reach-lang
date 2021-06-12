'reach 0.1';
'use strict';

export const main =
  Reach.App(
    {},
    [Participant('A', { get: Fun([], UInt) })],
    (A) => {
      A.only(() => {
        interact.get();
      });
    });
