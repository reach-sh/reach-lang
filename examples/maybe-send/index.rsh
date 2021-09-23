'reach 0.1';
'use strict';

export const main = Reach.App(
  {},
  [Participant('Alice', {
    getX: Fun([], UInt),
    getMx: Fun([], Maybe(UInt)),
  }), Participant('Bob', {
    showMx: Fun([Maybe(UInt)], Null),
    showMy: Fun([Maybe(UInt)], Null),
  })],
  (Alice, Bob) => {
    Alice.only(() => {
      const x = declassify(interact.getX());
    });
    Alice.publish(x);
    const [_, mx0] = [0, Maybe(UInt).None()];
    commit();

    Bob.publish();
    commit();
    Bob.only(() => {
      interact.showMx(mx0);
    });

    Alice.only(() => {
      const mx = declassify(interact.getMx());
      const my = (() => {
        switch (mx) {
        case Some: return Maybe(UInt).Some(mx + 1);
        case None: return Maybe(UInt).None();
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
