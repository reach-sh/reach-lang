'reach 0.1';

const ot = Object({a: UInt, b: UInt});
const otf = Object.fields(ot);
const newobj = Object({...otf, c: UInt});

export const main = Reach.App(() => {
  const A = Participant('A', {
    see: Fun([newobj], Null),
  });
  init();

  A.publish();
  commit();
  A.interact.see({a: 1, b: 2, c: 3});
  A.publish();
  commit();
});
