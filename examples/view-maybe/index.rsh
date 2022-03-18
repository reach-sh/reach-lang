'reach 0.1';

const MUInt = Maybe(UInt);
const Common = { checkView: Fun([MUInt], Null) };

export const main = Reach.App(() => {
  const A = Participant('Alice', Common);
  const B = Participant('Bob', Common);
  const vMain = View('Main', { i: UInt, });
  init();

  A.publish(); commit();
  A.only(() => interact.checkView(MUInt.None()));
  A.publish();
  vMain.i.set(1);
  A.only(() => interact.checkView(MUInt.Some(1)));
  commit();

  /*
  A.publish();
  vMain.i.set();
  A.only(() => interact.checkView(MUInt.None));
  commit();
  */

  A.publish();
  commit();
  B.only(() => interact.checkView(MUInt.None()));

  exit();
});
