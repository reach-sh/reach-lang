'reach 0.1';

function sum(...xs) {
  return Array.reduce(array(UInt, xs), 0, add);
}

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      const x = sum(1, 2, 3, 4, 5);
      assert(x == 15);
      const y = (...all) =>
        Array.reduce(array(UInt, all), 1, mul);
      assert(y(1, 2, 3) == 6);
    });
