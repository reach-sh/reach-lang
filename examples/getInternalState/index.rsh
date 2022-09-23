'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', { x: UInt, observe: Fun(true, Null) });
  const V = View({
    f: Fun([UInt], UInt)
  });
  init();

  A.only(() => {
    const x = declassify(interact.x);
  });
  A.publish(x);
  V.f.set(y => x + y);
  commit();

  A.interact.observe();
  A.publish();
  commit();

});
