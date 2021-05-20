'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', { x: UInt })],
    (A) => {
      require(A.interact.x == 5);
    });
