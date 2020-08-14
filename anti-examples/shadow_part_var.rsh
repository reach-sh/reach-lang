'reach 0.1';

export const main = Reach.App(
  {}, [["A", {y: UInt256}]], (A) => {
    A.only(() => {
      const y = declassify(interact.y);
    });
    const y = 1; // This should be an error, but isn't?
    A.only(() => {
      const z = y; // which y is this?
    });
    A.publish(z);
  }
);
