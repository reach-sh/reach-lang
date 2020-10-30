'reach 0.1';

export const main =
  Reach.App(
    {},
    [['A',{}]],
    (A) => {
      A.pay(0);
      assert(true, 4);
      commit();
      exit();
    } );
