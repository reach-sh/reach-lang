'reach 0.1';

export const main =
  Reach.App(
    {},
    [ Participant('A', { getA: Fun([], Tuple()) }),
      Participant('B', { getB: Fun([], Object({})) }),
      Participant('C', { getC: Fun([], Array(UInt, 0)) })
    ],
    (A, B, C) => {
      A.only(() => {
        const a = declassify(interact.getA()); });
      A.publish(a);
      commit();

      B.only(() => {
        const b = declassify(interact.getB()); });
      B.publish(b);
      commit();

      C.only(() => {
        const c = declassify(interact.getC()); });
      C.publish(c);
      commit();

      exit();
    });
