'reach 0.1';

export const main =
  Reach.App(
    {},
    [
      Participant('A', {
        i: UInt,
        show: Fun([UInt], Null)
      })
    ],
    (A) => {
      A.only(() => {
        const i = declassify(interact.i);
      })
      A.publish(i).pay(100);


      try {

        if (i < 5) {
          transfer(100).to(A);
          commit();
        } else {
          throw 100;
        }

      } catch (e) {
        transfer(e).to(A);
        commit();
      }

    });
