'reach 0.1';

const foo = () => {
  try {
    throw 5;
  } catch (e) {
    return 50;
  }
}

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      const x = foo();
      assert(x == 50);
    });
