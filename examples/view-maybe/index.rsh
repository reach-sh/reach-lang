'reach 0.1';

const MUInt = Maybe(UInt);

export const main = Reach.App(
  {},
  [ Participant('Alice', {
      checkView: Fun([MUInt], Null),
    }),
    View('Main', {
      i: UInt,
    }),
  ],
  (A, vMain) => {
    A.only(() => interact.checkView(MUInt.None()));
    A.publish();
    vMain.i.is(1);
    A.only(() => interact.checkView(MUInt.Some(1)));
    commit();

    /*
    A.publish();
    vMain.i.is();
    A.only(() => interact.checkView(MUInt.None));
    commit();
    */

    A.publish();
    vMain.i.is(3);
    A.only(() => interact.checkView(MUInt.None()));
    commit();

    exit();
  }
);
