'reach 0.1';

export const f = (x, y) => {
  require(x > y, "myDiv: x > y");
  return x / y;
};

export const myDiv = is(
  f,
  Refine(
    Fun([UInt, UInt], UInt),
    (([x, y]) => x > y),
    (_, _) => true
  )
);

export const main = Reach.App(() => {
  const A = Participant('A', { x: UInt, y: UInt });
  deploy();
  A.only(() => {
    const x = declassify(interact.x);
    const y = declassify(interact.y);
    assume(x > y, "local: x > y");
  });
  A.publish(x, y);
  const z = f(x, y);
  commit();
});
