'reach 0.1';

export const main =
  Reach.App(
    {},
    [ Participant('A', {}),
      Participant('B', {}),
      Participant('C', {})
    ],
    (A, B, C) => {
      A.pay(0);
      commit();

      B.pay(0);
      commit();

      C.pay(0);
      commit();

      exit();
    });
