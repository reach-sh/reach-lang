'reach 0.1';


function multiply (a, b = 1) {
  return a * b;
}

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      const x = multiply(5);
      assert(x==5);
    });
