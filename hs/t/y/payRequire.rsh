'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: Fun([], UInt),
    y: UInt
  });
  const B = API('B', {
    z: Fun([UInt], Null),
    z2: Fun([UInt], Null)
  });
  setOptions({ verifyArithmetic: true });
  init();

  A.only(() => {
    const x = declassify(interact.x());
    assume(x == 1);
  });

  A.publish(x)
    .pay(x, () => {
      require(x == 1);
    });

  transfer(1).to(A);
  commit();

  fork()
    .case(A,
      () => {
        const y = declassify(interact.y);
        assume(y == 1, "assume y == 1");
        return { when: true, msg: y };
      },
      [ (msg) => msg, (msg) => { require(msg == 1); } ],
      (_) => {})
    .case(A,
      () => {
        const x2 = declassify(interact.x());
        assume(x2 == 1, "assume x == 1");
        return { when: true, msg: x2 };
      },
      [ (msg) => msg, (msg) => { require(msg == 1); } ],
      (_) => {})
    .api(B.z,
      (msg) => { assume(msg == 1); },
      [ (msg) => msg, (msg) => { require(msg == 1); } ],
      (_, k) => {
        k(null);

        const keepGoing = parallelReduce(true)
          .while(keepGoing)
          .invariant(balance() == 1)
          .case(A,
            () => {
              const y = declassify(interact.y);
              assume(y == 1, "assume y == 1");
              return { when: true, msg: y };
            },
            [ (msg) => msg, (msg) => { require(msg == 1); } ],
            (_) => {
              transfer(1).to(A);
              return false;
            })
          .case(A,
            () => {
              const x2 = declassify(interact.x());
              assume(x2 == 1, "assume x == 1");
              return { when: true, msg: x2 };
            },
            [ (msg) => msg, (msg) => { require(msg == 1); } ],
            (_) => {
              transfer(1).to(A);
              return false;
            })
          .api(B.z2,
            (msg) => { assume(msg == 1); },
            [ (msg) => msg, (msg) => { require(msg == 1); } ],
            (_, k2) => {
              k2(null);
              transfer(1).to(A);
              return false;
            })
          .timeout(relativeSecs(10), () => {
            Anybody.publish();
            return false;
          });
      });

  transfer(1).to(A);
  commit();

});
