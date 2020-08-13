'reach 0.1';

export const main = Reach.App(
  {}, [["A", {x: UInt256}]], (A) => {
    const y = 0;
    A.only(() => {
      const _x = y;
      //         ^
      // 7:18:id ref: Invalid unbound identifier: y
    });
  }
);
