'reach 0.1';

export const main =
  Reach.App(
    {},
    [['A',{}]],
    (A) => {
      A.only(() => { const x = 1; });
      A.publish(x);
      assert(x == 1, 4);
      commit();
      exit();
    } );
