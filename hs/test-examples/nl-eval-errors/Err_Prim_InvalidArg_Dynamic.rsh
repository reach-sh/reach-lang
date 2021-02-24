'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', { x : UInt })],
    (A) => {
      A.only(() => {
        assert(sqrt(10, declassify(interact.x)) == 3);
      });
    }
  );
