'reach 0.1';

export const a = [1, 2, [3, 4, [5, 6, 7]]];

export const o = {
  a: 5,
  b: {
    c: true,
    d: [1, 2, [true, false]]
  }
}

const add1_impl = (x) => {
  assert(0 == x - x);
  return x + 1;
}

const add1_ty = Refine(
  Fun([UInt], UInt),
  (([x]) => x < 10),
  (([x], z) => x + 1 == z)
);

export const add1= is(add1_impl, add1_ty);

export const main = Reach.App(
  { verifyPerConnector: true },
  [Participant('Alice', {}), Participant('Bob', {})],
  (Alice, Bob) => {
    assert(add1(9) == 10);
    exit(); });
