'reach 0.1';

const a = 2;

export const foo = (x, y, z = (x + y + a), t = z - 2, u = x + x) =>
  x + y + z + t + u;

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      assert(foo(1, 2) == 13);
    });
