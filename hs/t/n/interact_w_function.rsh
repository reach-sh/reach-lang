'reach 0.1';

export const main =
  Reach.App(
    {},
    [['A', {
      // Can't shadow keywords within objects
      function: Fun([], Null)
    }]],
    (A) => {
      exit();
    });
