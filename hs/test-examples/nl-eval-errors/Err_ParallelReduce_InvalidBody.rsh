'reach 0.1';

export const main =
  Reach.App(
    {},
    [['Alice', {}]],
    (Alice) => {
      const x =
        parallel_reduce(0)
          .invariant(true)
          .case(Alice,
            () => {
              return 1; });

    });
