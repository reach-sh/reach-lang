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

  const _ = x.go(1);
  const _ = x.go2(3);
  const _ = x.go(2);

  commit();
});

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    deployed: Fun(  true, Null),
    // callGo  : Fun([UInt], Null),
    // callGo2 : Fun([UInt], Null),
  });
  const B = API(I);
  init();
  A.publish();
  A.interact.deployed();
  commit();

  A.publish();
  commit();

  // A.interact.callGo(1);
  const [ [x1], k1 ] = call(B.go).assume((x) => { check(x == 1); });
  check(x1 == 1);
  k1(1);
  commit();

  // A.interact.callGo2(3);
  const [ [x3], k3 ] = call(B.go2).assume((x) => { check(x == 3); });
  check(x3 == 3);
  k3(3);
  commit();

  // A.interact.callGo(2);
  const [ [x2], k2 ] = call(B.go).assume((x) => { check(x == 2); });
  check(x2 == 2);
  k2(2);
  commit();

});
