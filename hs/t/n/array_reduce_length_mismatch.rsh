'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    getA: Fun([], Array(UInt, 5)),
  });
  init();
  A.only(() => {
    const xs = declassify(interact.getA());
    const ys = array(UInt, [0, 1, 2]);
    const z = Array.reduce(xs, ys, 0, (ac, x, y) => ac - x + y);
  });
  A.publish(z);
  commit();
  exit();
});
