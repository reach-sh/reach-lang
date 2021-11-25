'reach 0.1';
'use strict';

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
          () => ({ when: declassify(interact.doCase(1)) }),
          (_) => payment,
          (_) => { return [ keepGoing ]; })
        .case(
          A,
          () => ({ when: declassify(interact.doCase(2)) }),
          (_) => payment * 2,
          (_) => { return [ keepGoing ]; })
        .case(
          A,
          () => ({ when: declassify(interact.doCase(3)) }),
          (_) => payment * 3,
          (_) => { return [ keepGoing ]; })
        .case(
          B,
          () => ({ when: declassify(interact.doCase(4)) }),
          (_) => payment * 4,
          (_) => { return [ keepGoing ]; })
        .timeout(relativeTime(timeout), () => {
          Anybody.publish();
          return [ false ];
        });

      transfer(balance()).to(B);
      commit();
    });
