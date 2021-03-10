'reach 0.1';

const makeFoo = (a) => (x, y, z = (x + y + a), t = z - 2, u = x + x) =>
  x + y + z + t + u;
const foo = makeFoo(2);

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      const a = 20;
      assert(foo(1, 2) == 13);
      void a;
    });
