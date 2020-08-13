'reach 0.1';

export const main = Reach.App(
  {}, [["A", {getX: Fun([], UInt256)}]], (A) => {
    A.only(() => {
      const _x = interact.getX();
      const y = 0;
      assert(_x > y);
    });
  }
);
