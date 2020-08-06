'reach 0.1';

export const main = Reach.App(
  {}, [["A", {}]], (A) => {
    A.only(() => {
      const _y = random();
    });
    // This should fail Eval because _y is private.
    A.publish(_y).pay(_y);
    transfer(_y).to(A);
    return _y;
  }
);
