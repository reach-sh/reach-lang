'reach 0.1';

export const main = Reach.App(
  {},
  [['A', {getX: Fun([],UInt256)}],
   ['B', {showX: Fun([UInt256], Null)}]],
  (A, B) => {
    A.only(() => {
      const aInteract = declassify(interact);
    });
    A.publish(aInteract);
    commit();
    B.only(() => {
      const _x = aInteract.getX();
      interact.showX(_x);
    });
  }
);
