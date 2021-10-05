'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', { getCtc: Fun([], Contract) })],
    (A) => {
      A.only(() => {
        const ctcInfo = declassify(interact.getCtc());
      });
      A.publish(ctcInfo);
      const ctc = remote(ctcInfo,  { f: Fun([], Null), });
      ctc.f.withBill(1)();
      commit();

    });
