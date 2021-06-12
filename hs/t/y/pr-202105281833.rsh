'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  deploy();
  const x = Array.replicate(3, 0);
  assert(x[0] == 0);
  assert(x[1] == 0);
  assert(x[2] == 0);
  assert(x.length == 3);
  assert(x == array(UInt, [0, 0, 0]));
  const y = x.set(1, 9);
  assert(y[0] == 0);
  assert(y[1] == 9);
  assert(y[2] == 0);
  assert(y.length == 3);
  assert(y == array(UInt, [0, 9, 0]));
  exit();
});
