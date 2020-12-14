'reach 0.1';

export const main =
  Reach.App(
    {},
    [['A', {}]],
    (A) => {
      fork()
      .case(A, (() => ({
        })),
        (a, b, c) => {
          commit();
          exit();
        });
    });
