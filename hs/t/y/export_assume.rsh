'reach 0.1';

export const myDiv = is(
  (x, y) => {
    assume(x > y, "myDiv: x > y");
    return x / y;
  },
  Fun([UInt, UInt], UInt)
);

export const main = Reach.App(() => {
  const A = Participant('A', { x: UInt, y: UInt });
  deploy();
  A.only(() => {
    const x = declassify(interact.x);
    const y = declassify(interact.y);
    const z = myDiv(x, y);
  });
  A.publish(z);
  commit();
});
