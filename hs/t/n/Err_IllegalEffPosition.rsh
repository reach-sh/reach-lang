'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      const x = A.pay(0);
      commit();
      exit();
    }
  );
