'reach 0.1';

const x = 0;

export const main = Reach.App(
  {}, [["A", {}]], (A) => {
    A.only(() => {
      const y = declassify(x);
    });
    A.publish(y);
    return y;
  }
);
