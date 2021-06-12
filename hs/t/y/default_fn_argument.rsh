'reach 0.1';


function myAdd (a, b = 1, c = 2) {
  return a + b + c;
}

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      assert(myAdd(5) == 8);
      assert(myAdd(5, 2) == 9);
      assert(myAdd(5, 2, 5) == 12);
    });
