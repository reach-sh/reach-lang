'reach 0.1';

const MMUInt = Maybe(Maybe(UInt));

export const main = Reach.App(
  {},
  [ Participant('Alice', {
      checkView: Fun([Address, MMUInt], Null),
    }),
    Participant('Bob', {}),
    View('Main', {
      f: Fun([Address], Maybe(UInt)),
    }),
  ],
  (A, B, vMain) => {
    A.only(() => interact.checkView(this, MMUInt.None()));

    A.publish();
    const intM = new Map(UInt);
    vMain.f.set((a) => intM[a]);
    const doCheck = (who) =>
      A.only(() => interact.checkView(who, MMUInt.Some(intM[who])));
    const failCheck = (who) =>
      A.only(() => interact.checkView(who, MMUInt.None()));
    doCheck(A);
    commit();

    B.publish();
    intM[A] = 0;
    doCheck(A);
    doCheck(B);
    commit();

    A.publish();
    intM[B] = 1;
    doCheck(A);
    doCheck(B);
    commit();

    A.publish();
    intM[A] = 2;
    doCheck(A);
    doCheck(B);
    commit();

    A.publish();
    delete intM[A];
    doCheck(A);
    doCheck(B);
    commit();

    A.publish();
    commit();

    failCheck(A);
    failCheck(B);
    exit();
  }
);
