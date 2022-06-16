'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      A.publish();
      commit();
      try {
        throw 10;
      } catch (error) {
        A.publish();
        commit();
        exit();
      }
    });