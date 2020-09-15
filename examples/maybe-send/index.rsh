'reach 0.1';

export const main = Reach.App(
  {},
  [['Alice', {
    getX: Fun([], UInt256),
    getMx: Fun([], Maybe(UInt256)),
  }], ['Bob', {
    showMx: Fun([Maybe(UInt256)], Null),
    showMy: Fun([Maybe(UInt256)], Null),
  }]],
  (Alice, Bob) => {
    Alice.only(() => {
      const x = declassify(interact.getX());
    });
    Alice.publish(x);
    var [i, mx0] = [0, Maybe(UInt256).None()];
    invariant(true);
    while (i < x) {
      [i, mx0] = [i + 1, Maybe(UInt256).Some(i + 1)];
      continue;
    }
    commit();

    Bob.only(() => {
      interact.showMx(mx0);
    });

    Alice.only(() => {
      const mx = declassify(interact.getMx());
      const my = (() => {
        switch (mx) {
        case Some: return Maybe(UInt256).Some(mx + 1);
        case None: return Maybe(UInt256).None();
        }
      })();
    });
    Alice.publish(mx, my);
    commit();

    Bob.only(() => {
      interact.showMx(mx);
      interact.showMy(my);
    });
  }
);
