'reach 0.1';

export const main =
  Reach.App(
    {},
    [['A', { getAddr: Fun([], Address) }],
     ['B', {}],
     ['C', {}]],
    (A, B, C) => {
      A.only(() => {
        const b = declassify(interact.getAddr()); });
      A.publish(b);
      B.set(b);
      commit();

      C.publish();
      Participant.set(B, b);
      commit();

      exit(); });
