'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    deployed: Fun([], Null),
    done: Fun([], Null),
  });
  const C = API({
    mul1: Fun([UInt], UInt),
    mul2: Fun([UInt, UInt], UInt),
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
