'reach 0.1';

export const main = Reach.App(
  {}, [["A", {x: UInt}]], (A) => {
    A.only(() => {
      const x = interact.x;
    });
  }
);
