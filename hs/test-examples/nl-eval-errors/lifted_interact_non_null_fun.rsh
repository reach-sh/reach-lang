'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', { f: Fun([], UInt) })],
    (A) => {
      A.interact.f();
    });
