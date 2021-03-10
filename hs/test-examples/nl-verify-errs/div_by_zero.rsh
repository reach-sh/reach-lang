'reach 0.1';

export const main =
  Reach.App(
    { verifyArithmetic: true },
    [Participant('A', { x: UInt })],
    (A) => {
      A.only(() => {
        const _ = 5 / declassify(interact.x);
      });
    });
