'reach 0.1';

export const main =
  Reach.App(
    {},
    [['A', {}]],
    (A) => {
      A.publish();
      commit();

      A.publish()
      .timeout(lastConsensusTime() + 10, () => {
        exit(); });

      exit();
    });
