'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      A.pay(0).timeout(1);
      commit();
      exit();
    });
