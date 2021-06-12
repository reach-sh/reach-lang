'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      const f = (a = 1, b) => a + b;
      assert(f(2, 3) == 5);
    });
