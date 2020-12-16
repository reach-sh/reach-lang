'reach 0.1';

export const main =
  Reach.App(
    {},
    [['A', {}]],
    (A) => {
      fork()
      .case(A, (() => ({})),
        () => { commit(); exit(); })
      .case(A, (() => ({})),
        () => { commit(); exit(); });
    });
