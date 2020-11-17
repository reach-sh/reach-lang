'reach 0.1';

export const main =
  Reach.App(
    {},
    [['A', {}]],
    (A) => {
      const {a: { b = 5 }} = { a: {} };
      assert(b == 5);
    });
