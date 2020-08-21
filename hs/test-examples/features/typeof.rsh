'reach 0.1';

// typeof as a unary operator
const XTy = typeof 0;

// typeOf as a function
const YTy = typeOf('hello');

export const main = Reach.App(
  {},
  [['A', {getX: Fun([], XTy),
          getY: Fun([], YTy)}]],
  (A) => {
    A.only(() => {
      const _x = interact.getX();
      const _b = _x == 1;
      assert(typeOf(_x) == XTy);
      assert(typeOf(_b) == Bool);

      const _y = interact.getY();
      const _c = _y == 'okay';
      assert(typeOf(_y) == YTy);
      assert(typeOf(_c) == Bool);

      // UInt256 != Bytes
      assert(XTy != YTy);

      // Type(UInt256) != Type(Bytes)
      assert(typeOf(XTy) != typeOf(YTy));

      assert(XTy == UInt256);

      const XTyTy = typeOf(XTy);
      assert(XTyTy == typeOf(UInt256));
      assert(XTyTy != UInt256);
    });
  }
);
