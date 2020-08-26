'reach 0.1';

export const main = Reach.App(
  {}, [['A', {x: Bytes}]], (A) => {
    A.only(() => {
      const x = declassify(interact.x);
      assume(bytes_eq(x, 'x'));
    });
    A.publish(x);
    require(bytes_eq(x, 'x'));
  }
);
