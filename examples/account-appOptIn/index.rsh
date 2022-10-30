'reach 0.1';
export const main = Reach.App(() => {
  const A = Participant('A', {ready: Fun([], Null)});
  const B = Participant('B', {});
  init();
  A.publish();
  const m = new Map(Address, UInt);
  m[A] = 1;
  A.interact.ready();
  commit();
  B.publish();
  m[B] = 2;
  commit();
  A.publish();
  commit();
});
