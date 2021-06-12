'reach 0.1';

// typeof as a unary operator
const XTy = typeof 0;

// typeOf as a function
const YTy = typeOf(true);

export const main = Reach.App(
  {},
  [Participant('A', {getX: Fun([], XTy),
          getY: Fun([], YTy)})],
  (A) => {
    A.only(() => {
      const _x = interact.getX();
      const _b = _x == 1;
      assert(typeOf(_x) == XTy);
      assert(typeOf(_b) == Bool);

      const _y = interact.getY();
      const _c = _y == false;
      assert(typeOf(_y) == YTy);
      assert(typeOf(_c) == Bool);

      // UInt != Bytes
      assert(XTy != YTy);

      // Type(UInt) != Type(Bytes)
      assert(typeOf(XTy) != typeOf(YTy));

      assert(XTy == UInt);

      const XTyTy = typeOf(XTy);
      assert(XTyTy == typeOf(UInt));
      assert(XTyTy != UInt);
    });
  }
);
