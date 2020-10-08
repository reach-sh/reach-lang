'reach 0.1';

export const main = Reach.App(
  {},
  [['A', {getX: Fun([],UInt)}],
   ['B', {showX: Fun([UInt], Null)}]],
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
