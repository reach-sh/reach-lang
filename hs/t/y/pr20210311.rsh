'reach 0.1';

const Common = {
  getX: Fun([], UInt),
  seeX: Fun([UInt], Null),
};

export const main =
  Reach.App(
    {},
    [ Participant('A', Common),
      Participant('B', Common),
    ],
    (A, B) => {
      A.publish();
      commit();
      B.publish();

      var round = 0;
      invariant( balance() == 0 );
      while ( round < 8 ) {
        const doRound = (First, Second) => {
          commit();

          First.only(() => {
            const x = declassify(interact.getX()); });
          First.publish(x);
          commit();

          Second.only(() => {
            const y = x;
            interact.seeX(x); });
          Second.publish(y);
        };

        if ( round % 2 == 0 ) {
          doRound(A, B);
        } else {
          doRound(B, A);
        }

        round = round + 1;
        continue;
      }
      commit();

      exit();
    }
  );
