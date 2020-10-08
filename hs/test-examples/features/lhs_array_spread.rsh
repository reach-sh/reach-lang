'reach 0.1';

export const main = Reach.App(
  {},
  [['A', { get: Fun([], Tuple(UInt, Bool, UInt)),
           put: Fun([UInt, Bool, Tuple(UInt)], Null) }]],
  (A) => {
    const [ t, ...foo ] = [ 0 ];
    assert(t == 0);
    assert(foo.length == 0);

    const xs = [0, true, 2, 3];
    assert(xs.length == 4);

    const [a, b, ...c] = xs;
    assert(a == 0);
    assert(b == true);
    assert(c.length == 2);
    assert(c[0] == 2);
    assert(c[1] == 3);

    A.only(() => {
      const [x, y, ...z] = declassify(interact.get());
      interact.put(x, y, z);
    });
  }
);
