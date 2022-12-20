'reach 0.1';

const MUInt = Maybe(UInt);
const MMUInt = Maybe(MUInt);
const Common = { checkView: Fun([UInt, Address, MMUInt, MUInt], Null) };

export const main = Reach.App(() => {
  setOptions({ untrustworthyMaps: true });
  const A = Participant('Alice', Common);
  const B = Participant('Bob', Common);
  const vMain = View('Main', {
    f: Fun([Address], MUInt),
    g: Fun([Address], UInt),
  });
  init();
  A.publish(); commit();

  A.only(() => interact.checkView(0, this, MMUInt.None(), MUInt.None()));

  A.publish();
  const intM = new Map(UInt);
  vMain.f.set((a) => intM[a]);
  vMain.g.set((a) => fromSome(intM[a], 0));
  const doCheck = (x, who) => {
    const z = intM[who];
    A.interact.checkView(x, who, MMUInt.Some(z), MUInt.Some(fromSome(z, 0)));
  };
  const failCheck = (x, who) =>
    B.interact.checkView(x, who, MMUInt.None(), MUInt.None());
  doCheck(1, A);
  commit();

  A.publish();
  commit();

  B.publish();
  intM[A] = 0;
  doCheck(2, A);
  doCheck(3, B);
  commit();

  A.publish();
  intM[B] = 1;
  doCheck(4, A);
  doCheck(5, B);
  commit();

  A.publish();
  intM[A] = 2;
  doCheck(6, A);
  doCheck(7, B);
  commit();

  A.publish();
  delete intM[A];
  doCheck(8, A);
  doCheck(9, B);
  commit();

  A.publish();
  delete intM[B];
  commit();

  failCheck(10, A);
  failCheck(11, B);
  exit();
});
