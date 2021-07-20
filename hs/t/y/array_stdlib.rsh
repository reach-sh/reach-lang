'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      A.only(() => {
        const a = Array.iota(5);
        assert(a.sum() == 10);
        assert(a.min() == 0);
        assert(a.max() == 4);
        assert(a.count(x => x < 3) == 3);
        assert(Array.any(a, x => x < 2));
        assert(!Array.all(a, x => x < 2));
        assert(a.findIndex(x => x == 2) == Maybe(UInt).Some(2));
        assert(a.indexOf(15) == Maybe(UInt).None());
        assert(Array.and(array(Bool, [true, true])));
        assert(Array.or(array(Bool, [true, false])));
        assert(a.includes(2));
        assert(!a.includes(322));
        assert(a.mapWithIndex((x, i) => x - i).all(x => x == 0));
        assert(a.elemType == UInt);
        assert(Array.find(a, (p) => p > 5) == Maybe(UInt).None());
        assert(a.find((p) => p > 0) == Maybe(UInt).Some(1));
        assert(a.reduceWithIndex(0, (acc, e, idx) => acc + e + idx) == 20);
        assert(array(Bool, [false, true]).withIndex() == array(Tuple(Bool, UInt), [[false, 0], [true, 1]]));
        assert(a.slice(2, 2) == array(UInt, [2, 3]));
      });
    });
