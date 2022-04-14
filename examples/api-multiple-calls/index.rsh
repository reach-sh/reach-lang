'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    deployed: Fun(true, Null),
  });
  const B = API({
    go: Fun([UInt], UInt),
    go2: Fun([UInt], UInt),
  });
  init();
  A.publish();
  A.interact.deployed();
  commit();

  const [ [x1], k1 ] = call(B.go).assume((x) => { check(x == 1); });
  check(x1 == 1);
  k1(1);
  commit();

  const [ [x2], k2 ] = call(B.go).assume((x) => { check(x == 2); });
  check(x2 == 2);
  k2(2);
  commit();

  const [ [x3], k3 ] = call(B.go2).assume((x) => { check(x == 3); });
  check(x3 == 3);
  k3(3);
  commit();

});
