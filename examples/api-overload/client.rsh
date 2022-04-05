'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    getCtc: Fun([], Contract)
  });
  init();

  A.only(() => {
    const ctc = declassify(interact.getCtc());
  });
  A.publish(ctc);

  const preCondish = (_) => true;

  const postCondish = (args, z) => array(UInt, args).product() == z;

  const calc = remote(ctc, {
    mul1: Refine(Fun([UInt], UInt), preCondish, postCondish),
    mul2: Refine(Fun([UInt, UInt], UInt), preCondish, postCondish),
  }, {
    mul1: "mul",
    mul2: "mul",
  });

  const x = calc.mul1(2);
  check(x == 2);

  commit();

  A.publish();

  const y = calc.mul2(4, 6);
  check(y == 24);

  commit();

});
