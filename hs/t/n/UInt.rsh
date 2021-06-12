'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      A.only(() => {
        const UInt = 5;
      });
    });
