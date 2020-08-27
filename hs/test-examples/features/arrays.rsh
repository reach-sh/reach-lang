'reach 0.1';

export const main = Reach.App(
  {}, [['A', {get: Fun([],Tuple(UInt256, Bool, UInt256))}]], (A) => {
    const xs = [0, true, 2, 3];
    assert(xs[1] == true);

    const xs2 = xs.set(1, false);
    assert(xs2[1] == false);

    A.only(() => {
      const _xs3 = interact.get();
      const xs4 = declassify(tuple_set(_xs3, 1, 1)); });
    A.publish(xs4);
    commit();

  }
);
