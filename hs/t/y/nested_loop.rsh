'reach 0.1';

const common = {
  shouldGo: Fun([], Bool),
};

export const main =
  Reach.App(
    {},
    [ Participant('A', { ...common, timeout: UInt })
    , ParticipantClass('B', { ...common })],
    (A, B) => {
      A.only(() => {
        const timeout = declassify(interact.timeout);
      });
      A.publish(timeout);

      var it = A;
      invariant(balance() == 0);
      while (true) {

        commit();
        A.pay(10);
        commit();
        B.pay(10);

        const [ timeRemaining, keepGoing ] = makeDeadline(timeout);

        const [ newIt ] =
          parallelReduce([ it ])
            .invariant(balance() == 20)
            .while(keepGoing ())
            .case(A,
              (() => ({ when : declassify(interact.shouldGo()) })),
              (() => { return [ this ]; })
            )
            .case(B,
              (() => ({ when : declassify(interact.shouldGo()) })),
              (() => { return [ this ]; })
            )
            .timeRemaining(timeRemaining());

        commit();
        A.publish();

        transfer(balance()).to(newIt);

        it = newIt;
        continue;
      }
      commit();

    });
