'reach 0.1';

export const a = [1, 2, [3, 4, [5, 6, 7]]];

export const o = {
  a: 5,
  b: {
    c: true,
    d: [1, 2, [true, false]]
  }
}

const add1_impl = (x) => x + 1;

export const add1 = is(add1_impl, Fun([UInt], UInt));

const sumMul2_impl = (x, y) => {
  assert (x < y);
  return (x >= y)
    ? 0
    : ((x + y) * 2);
}

const sumMul2_ty = Refine(
  Fun([UInt, UInt], UInt),
  (([x, y]) => x < y),
  (([x, y], z) => x + y < z)
);

export const sumMul2 = is(sumMul2_impl, sumMul2_ty);

export const main = Reach.App(
  { },
  [Participant('Alice', { x:UInt, y:UInt }), Participant('Bob', {})],
  (Alice, Bob) => {
    assert(add1(9) == 10);
    Alice.only(() => {
      const x = declassify(interact.x);
      const y = declassify(interact.y);
      assume(x < y);
      assert(sumMul2(x, y) > x + y);
    })
    exit(); });
