'reach 0.1';

const mul1Ty = Fun([UInt], UInt);
const mul2Ty = Fun([UInt, UInt], UInt);

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    deployed: Fun([], Null),
    done: Fun([], Null),
  });
  const C = API({
    mul1: mul1Ty,
    mul2: mul2Ty,
  }, {
    mul1: "mul",
    mul2: "mul",
  });
  init();

  A.publish();
  A.interact.deployed();
  commit();

  const [ [ z ], k1 ] = call(C.mul1);
  k1(z * 1);
  commit();

  const [ [ x, y ], k2 ] = call(C.mul2);
  k2(x * y);
  commit();

  A.publish();
  A.interact.done();
  commit();

});

export const client = Reach.App(() => {
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
    mul1: Refine(mul1Ty, preCondish, postCondish),
    mul2: Refine(mul2Ty, preCondish, postCondish),
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

