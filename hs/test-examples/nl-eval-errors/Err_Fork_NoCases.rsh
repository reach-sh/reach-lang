'reach 0.1';

export const main =
  Reach.App(
    {},
    [['Alice', {}]],
    (Alice) => {
      fork();
    });

