'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      fork()
      .case(A, (() => ({
        })),
        (a, b, c) => {
          commit();
          exit();
        });
    });
