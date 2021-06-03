'reach 0.1';

const N = 10;
export const main = Reach.App(() => {
  const A = Participant('A', {
    get: Fun([], UInt),
    arr: Fun([], Array(UInt, N)),
    put: Fun(true, Null),
  });
  deploy();

  assert(1 < 2 ? true : false);
  assert(2 < 1 ? false : true);

  A.only(() => {
    const a0 = declassify(interact.arr());
    const n0 = declassify(interact.get()); });
  A.publish(a0, n0);

  const check = (n) => {
    const a1 = (n0 < a0.length) ? a0.set(n0, 1) : a0;
    assert(a1[0] == a0[0] || a1[0] == 1);
    assert(n0 == 0 ? a1[0] == 1 : true);

    const a2 = (() => {
      if (n0 < a0.length) {
        return a0.set(n0, 1);
      } else {
        return a0;
      }
    })();
    assert(a2[0] == a0[0] || a2[0] == 1);
    assert(n0 == 0 ? a2[0] == 1 : true);

    return [a0, a1, a2];
  };

  const x0 = check(n0);
  const x1 = check(0);
  const x2 = check(N - 2);

  commit();

  A.interact.put([x0, x1, x2]);

  exit();
});
