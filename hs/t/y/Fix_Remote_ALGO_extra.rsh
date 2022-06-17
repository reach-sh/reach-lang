'reach 0.1';
export const main = Reach.App(() => {
  const A = Participant('A', { c: Contract });
  init();

  A.only(() => {
    const c = declassify(interact.c);
  });
  A.publish(c);
  const x = remote(c, { f: Fun([], UInt) }).f.ALGO({});
  commit();

  exit();
});
