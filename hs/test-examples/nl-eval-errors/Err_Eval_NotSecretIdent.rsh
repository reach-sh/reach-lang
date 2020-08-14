'reach 0.1';

export const main = Reach.App(
  {}, [["A", {x: UInt256}]], (A) => {
    A.only(() => {
      const x = interact.x;
    });
  }
);
