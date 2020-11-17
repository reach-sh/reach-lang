'reach 0.1';

export const main =
  Reach.App(
    {},
    [['Alice', {}]],
    (Alice) => {
      const f = () => { exit(); };

      f();
      const x =
        parallel_reduce(0)
          .invariant(true);

    });
