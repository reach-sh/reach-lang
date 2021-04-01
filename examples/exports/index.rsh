'reach 0.1';

export const a = [1, 2, [3, 4, [5, 6, 7]]];

export const o = {
  a: 5,
  b: {
    c: true,
    d: [1, 2, [true, false]]
  }
}

export const add1_impl = (x) => x + 1;

export const add1= is(add1_impl, Fun([UInt], UInt));

export const main = Reach.App(
  {}, [Participant('Alice', {}), Participant('Bob', {})], (Alice, Bob) => {
    assert(add1(1) == 2);
    exit(); });
