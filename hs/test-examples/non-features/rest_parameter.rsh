'reach 0.1';

function sum(...xs) {
  return Array.fold(xs, 0, add);
}

export const main =
  Reach.App(
    {},
    [['A', {}]],
    (A) => {
      sum(1, 2, 3, 4, 5);
    });
