'reach 0.1';

export const main = Reach.App(
  {}, [["A", {x: UInt256}]], (A) => {
    const y = 0;
    A.only(() => {
      const _x = interact.x;
      possible(_x < y);
      //            ^
      // 8:21:id ref: Invalid unbound identifier: y
    });
  }
);
