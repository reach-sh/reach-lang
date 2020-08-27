'reach 0.1';

export const main =
  Reach.App(
    {},
    [['A', {get: Fun([],Tuple(UInt256, Bool, UInt256))}]],
    (A) => {
      assert('x' != 'y');
      A.only(() => {
        const _x = interact.get();
        assert(_x[0] == _x[0]); }); });
