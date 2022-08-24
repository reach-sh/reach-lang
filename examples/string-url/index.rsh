'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    base: StringDyn,
    ready: Fun([Contract], Null),
  });
  const V = View({ read: Fun([UInt], StringDyn) });
  init();
  A.only(() => { const base = declassify(interact.base); });
  A.publish(base);
  V.read.set((i) => StringDyn.concat(base, StringDyn.concat(StringDyn(i), StringDyn(".jpg"))));
  A.interact.ready(getContract());
  commit();
  A.publish();
  commit();
  exit();
});
