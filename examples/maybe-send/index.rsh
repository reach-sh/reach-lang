'reach 0.1';

export const main = Reach.App(
  {},
  [['Alice', {getMx: Fun([], Maybe(UInt256))}],
   ['Bob', {showMx: Fun([Maybe(UInt256)], Null)}]],
  (Alice, Bob) => {
    Alice.only(() => {
      const mx = declassify(interact.getMx());
    });
    Alice.publish(mx);
    commit();

    Bob.only(() => {
      interact.showMx(mx);
    });
  }
);
