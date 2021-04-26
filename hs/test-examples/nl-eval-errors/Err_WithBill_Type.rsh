'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', { getCtc: Fun([], Address) })],
    (A) => {
      A.only(() => {
        const addr = declassify(interact.getCtc());
      });
      A.publish(addr);
      const ctc = remote(addr,  { f: Fun([], Null), });
      ctc.f.withBill(1)();
      commit();

    });
