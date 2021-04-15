'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {
      timeout: UInt,
      doCase1: Fun([], Bool),
      doCase2: Fun([], Bool),
      show   : Fun([UInt], Null)
    })],
    (A) => {
      A.only(() => {
        const timeout = declassify(interact.timeout); });
      A.publish(timeout);
      commit();

      try {
        fork()
          .case(
            A,
            (() => ({ when: declassify(interact.doCase1()) })),
            () => {
              commit();
              A.only(() => interact.show(1));
            })
          .case(
            A,
            (() => ({ when: declassify(interact.doCase2()) })),
            () => {
              commit();
              A.only(() => interact.show(2));
            })
          .throwTimeout(timeout, true)

      } catch(didTimeout) {
        assert(didTimeout);
        Anybody.publish();
        commit();
      }
      exit();

    });
