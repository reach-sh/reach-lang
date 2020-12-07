'reach 0.1';

export const main =
  Reach.App(
    {},
    [['Alice', { x: UInt }], ['Bob', { x: UInt }]],
    (A, B) => {
      A.only(() => {
        const ax = declassify(interact.x);
      });
      A.publish(ax);
      commit();

      B.only(() => {
        const bx = declassify(interact.x);
        const bi = 45 + bx;
      });
      B.publish(bi);
      commit();

      exit(); });
