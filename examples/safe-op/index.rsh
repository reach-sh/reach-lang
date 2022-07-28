'reach 0.1';

export const mkTest = (f) => Reach.App(() => {
  const A = Participant('A', { x: UInt, bx: UInt256, show: Fun(true, Null) });
  setOptions({ verifyArithmetic: true });
  init();
  f(A);
});

export const safeAddTest = mkTest((A) => {
  A.only(() => {
    const x = declassify(interact.x); });
  A.publish(x);
  const z = safeAdd(x, 5);
  assert(z <= UInt.max);
  A.interact.show(z);
  commit();
});

export const safeSubTest = mkTest((A) => {
  A.only(() => {
    const x = declassify(interact.x); });
  A.publish(x);
  const z = safeSub(x, 5);
  A.interact.show(z);
  commit();
});

const mkDivLikeTest = (f) => mkTest((A) => {
  A.only(() => {
    const x = declassify(interact.x); });
  A.publish(x);
  const z = f(5, x);
  A.interact.show(z);
  commit();
});

export const safeDivTest = mkDivLikeTest(safeDiv);
export const safeModTest = mkDivLikeTest(safeMod);

export const safeCastTest = mkTest((A) => {
  A.only(() => {
    const bx = declassify(interact.bx);
  });
  A.publish(bx);
  const y = UInt(bx, false, false);
  A.interact.show(y);
  commit();
});