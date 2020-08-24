'reach 0.1';

export const main = Reach.App(
  {}, [['A', {}]], (A) => {
    // false should instead be a "call-like" expression
    unknowable(A, false);
  }
);
