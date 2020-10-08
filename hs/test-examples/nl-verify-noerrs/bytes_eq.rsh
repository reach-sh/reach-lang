'reach 0.1';

export const main =
  Reach.App(
    {},
    [['A', {get: Fun([],Tuple(UInt, Bool, UInt))}]],
    (A) => {
      assert('x' != 'y');
      A.only(() => {
        const _x = interact.get();
        assert(_x[0] == _x[0]); }); });
