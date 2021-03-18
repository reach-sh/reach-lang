'reach 0.1';

const Common = ({
  doCase: Fun([UInt], Bool) });

export const main =
  Reach.App(
    {},
    [
      Participant('A', { timeout: UInt, payment: UInt, ...Common }),
      Participant('B', { ...Common }),
    ],
    (A, B) => {
      A.only(() => {
        const timeout = declassify(interact.timeout);
        const payment = declassify(interact.payment) });
      A.publish(timeout, payment);
      commit();
      B.publish();

      const [ keepGoing ] =
        parallelReduce([ true ])
        .while(keepGoing)
        .invariant(balance() == balance())
        .case(
          A,
          (() => ({ when: declassify(interact.doCase(1)) })),
          (() => payment),
          () => { return [ keepGoing ]; })
        .case(
          A,
          (() => ({ when: declassify(interact.doCase(2)) })),
          (() => payment * 2),
          () => { return [ keepGoing ]; })
        .case(
          A,
          (() => ({ when: declassify(interact.doCase(3)) })),
          (() => payment * 3),
          () => { return [ keepGoing ]; })
        .case(
          B,
          (() => ({ when: declassify(interact.doCase(4)) })),
          (() => payment * 4),
          () => { return [ keepGoing ]; })
        .timeout(timeout, () => {
          Anybody.publish();
          return [ false ];
        });

      transfer(balance()).to(B);
      commit();
    });
