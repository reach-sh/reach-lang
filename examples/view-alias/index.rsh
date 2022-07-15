'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    published: Fun([], Null),
    checkView: Fun([], Null),
  });
  const V1 = View({
    x: UInt,
    f: Fun([UInt], UInt)
  }, {
    // ensure key is in view interface
    x: ["val", "val2"],
    f: ["succ"],
  });
  const V2 = View('V2', {
    y: UInt,
    g: Fun([UInt], UInt)
  }, {
    y: ["yVal"],
    g: ["sub"],
  });
  init();

  A.publish();
  V1.x.set(43);
  V1.f.set((x) => x + 1);
  V2.y.set(43);
  V2.g.set((x) => x + 1);
  commit();

  A.interact.checkView();
  A.publish();
  commit();

});
