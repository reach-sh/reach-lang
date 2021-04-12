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


      // Eval `try` in `consensus step`
      try {
        // Change state within try block
        commit();
        // Catch block should recognize it's executing in `step` mode
        throw 10;

      } catch (e) {
        A.publish();
        transfer(e).to(A);
        commit();
      }
      // ^^^ Both `try` block and `catch` block completed in `step` mode

      A.publish();

      try {
        // Either case we should distribute the remaining balance
        // `try` block is in `consensus step`
        if (i < 5) {
          transfer(90).to(A);
          commit();
        } else {
          // `catch` block should execute in `consensus step` not `step`
          throw 90;
        }
      } catch (e) {
        transfer(e).to(A);
        commit();
      }

      each([A], () => {
        interact.show(5);
      });

    });
