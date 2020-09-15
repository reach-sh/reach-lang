'reach 0.1';

export const main = Reach.App(
  {},
  [['Alice', {getMx: Fun([], Maybe(UInt256))}],
   ['Bob', {
     showMx: Fun([Maybe(UInt256)], Null),
     showMy: Fun([Maybe(UInt256)], Null),
   }]],
  (Alice, Bob) => {
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
