'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', { log: Fun([UInt], Null) })],
    (A) => {
      A.interact.log(5);
    });
