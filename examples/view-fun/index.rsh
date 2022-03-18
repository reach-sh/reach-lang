'reach 0.1';

const MUInt = Maybe(UInt);
const Common = { checkView: Fun([UInt, MUInt], Null) };

export const main = Reach.App(() => {
  const A = Participant('Alice', Common);
  const B = Participant('Bob', Common);
  const vMain = View('Main', { f: Fun([UInt], UInt) });
  init();

  A.publish(); commit();
  A.interact.checkView(0, MUInt.None());
  A.interact.checkView(1, MUInt.None());
  A.interact.checkView(2, MUInt.None());

  A.publish();
  vMain.f.set((x) => x + 1);
  A.interact.checkView(0, MUInt.Some(1));
  A.interact.checkView(1, MUInt.Some(2));
  A.interact.checkView(2, MUInt.Some(3));
  commit();

  A.publish();
  vMain.f.set((x) => x - 1);
  A.interact.checkView(0, MUInt.None());
  A.interact.checkView(1, MUInt.Some(0));
  A.interact.checkView(2, MUInt.Some(1));
  commit();

  A.publish();
  commit();

  B.interact.checkView(0, MUInt.None());
  B.interact.checkView(1, MUInt.None());
  B.interact.checkView(2, MUInt.None());

  exit();
});
