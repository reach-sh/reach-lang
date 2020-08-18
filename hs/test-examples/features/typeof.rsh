'reach 0.1';

// typeof as a unary operator
const XTy = typeof 0;

// typeOf as a function
const YTy = typeOf("hello");

export const main = Reach.App(
  {},
  [["A", {getX: Fun([], XTy),
          getY: Fun([], YTy)}]],
  (A) => {
    A.only(() => {
      const _b = interact.getX() == 1;
      const _c = interact.getY() === "okay";
    });
  }
);
