'reach 0.1';

const MMUInt = Maybe(Maybe(UInt));

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    checkView: Fun([UInt, Address, MMUInt], Null),
  });
  const B = Participant('Bob', {});
  const vMain = View('Main', {
    f: Fun([Address], Maybe(UInt)),
  });
  deploy();
  A.publish(); commit();

  A.only(() => interact.checkView(0, this, MMUInt.None()));

  A.publish();
  const intM = new Map(UInt);
  vMain.f.set((a) => intM[a]);
  const doCheck = (x, who) =>
    A.only(() => interact.checkView(x, who, MMUInt.Some(intM[who])));
  const failCheck = (x, who) =>
    A.only(() => interact.checkView(x, who, MMUInt.None()));
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
  commit();

  failCheck(10, A);
  failCheck(11, B);
  exit();
});
