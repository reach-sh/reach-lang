'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    mid: Fun([], Null),
  });
  const B = Participant('B', {});
  setOptions({untrustworthyMaps: true});
  init();
  A.publish();
  const m = new Map(UInt);
  m[A] = 1;
  commit();
  B.publish();
  m[A] = 2;
  m[B] = 3;
  commit();
  A.interact.mid();
  A.publish();
  m[A] = 4;
  m[B] = 5;
  commit();
  A.publish();
  delete m[A];
  delete m[B];
  commit();
  exit();
});
