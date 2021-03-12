'reach 0.1';

export const main =
  Reach.App(
    {},
    [ Participant('A', {
        getA: Fun([], Array(UInt, 5)),
      }),
    ],
    (A) => {
      A.only(() => {
        const x = declassify(interact.getA().length);
        const y = array(UInt, [0, 1, 2]).length;
        assert(x == 5);
        assert(y == 3);
      });
      A.publish(x, y);
      commit();
      exit();
    }
  );
