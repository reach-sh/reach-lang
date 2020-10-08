'reach 0.1';

export const main = Reach.App(
  {},
  [['A', {getX: Fun([], UInt)}]],
  (A) => {
    A.only(() => {
      const _x1 = interact.getX();
      const _x2 = interact.getX();
      const _2x = _x1 * 2;
      assert(_x1 + _x2 == _2x);
    });
  }
);
