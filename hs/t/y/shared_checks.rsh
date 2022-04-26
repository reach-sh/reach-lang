'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const I = { x: UInt };
  const A = Participant('A', I);
  const B = Participant('B', I);
  init();

  A.publish();
  commit();

  A.only(() => {
    const x = declassify(interact.x); });
  B.only(() => {
    const x = declassify(interact.x); });

  race(A, B).publish(x).check(() => {
    check(x > 0, "x > 0");
  });

  commit();
  assert(x > 0, "x > 0");
});
