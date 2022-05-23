'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      try {
        throw 10;
      } catch (error) {
        exit();
      }
    });
