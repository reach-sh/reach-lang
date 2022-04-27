'reach 0.1';

const I = {
  go : Fun([UInt], UInt),
  go2: Fun([UInt], UInt),
}

export const client = Reach.App(() => {
  const A = Participant('A', {
    ctc : Fun([], Contract),
  });
  init();

  A.only(() => {
    const ctc = declassify(interact.ctc());
  });
  A.publish(ctc);
  const x = remote(ctc, I);
  commit();

  A.publish();
  void x.go(1);
  commit();

  A.publish();
  void x.go2(3);
  commit();

  A.publish();
  void x.go(2);
  commit();

});

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    deployed: Fun(  true, Null),
    done    : Fun([], Null),
  });
  const B = API(I);
  init();
  A.publish();
  A.interact.deployed();
  commit();

  A.publish();
  commit();

  const [ [x1], k1 ] = call(B.go).assume((x) => { check(x == 1); });
  check(x1 == 1);
  k1(1);
  commit();

  const [ [x3], k3 ] = call(B.go2).assume((x) => { check(x == 3); });
  check(x3 == 3);
  k3(3);
  commit();

  const [ [x2], k2 ] = call(B.go).assume((x) => { check(x == 2); });
  check(x2 == 2);
  k2(2);
  commit();

  A.interact.done();

  A.publish();
  commit();

});
