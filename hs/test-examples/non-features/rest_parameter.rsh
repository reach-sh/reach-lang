'reach 0.1';

function sum(...xs) {
  return Array.fold(xs, 0, add);
}

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      const x = sum(1, 2, 3, 4, 5);
      assert(x == 15);
    });
