'reach 0.1';

const Common = ({
  doCase: Fun([UInt], Bool)
});

export const main = Reach.App(
  {}, [Participant('A', { ...Common }), Participant('B', { ...Common })], (A, B) => {
    A.publish()
      .pay(1);
    commit();

    B.publish();

    const keepGoing =
      parallelReduce(true)
        .invariant(balance() == 1)
        .while(keepGoing)
        .case(
          A,
          (() => ({ when: true })),
          ((_) => {

            const keepGoingB =
              parallelReduce(true)
                .invariant(balance() == 1)
                .while(keepGoingB)
                .case(
                  A,
                  (() => ({ when: declassify(interact.doCase(1)) })),
                  () => { return true; })
                .timeout(1000, () => {
                  Anybody.publish();
                  return false;
                });

            return false;

          })
        )
        .timeout(false);

    transfer(1).to(B);
    commit();
    exit();
  }
);

