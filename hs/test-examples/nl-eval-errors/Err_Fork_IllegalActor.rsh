'reach 0.1';

export const main =
  Reach.App(
    {},
    [['Alice', {}], ['Bob', {}]],
    (Alice, Bob) => {
      fork()
      .case(Alice, () => {
        Bob.only(() => {
          const x = 5; });
        Bob.publish(x);
        commit(); });
    });
