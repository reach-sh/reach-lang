'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', { getAddr: Fun([], Address) }),
     Participant('B', {}),
     Participant('C', {})],
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
