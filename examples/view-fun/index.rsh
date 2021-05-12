'reach 0.1';

const MUInt = Maybe(UInt);

export const main = Reach.App(
  {},
  [ Participant('Alice', {
      checkView: Fun([UInt, MUInt], Null),
    }),
    View('Main', {
      f: Fun([UInt], UInt),
    }),
  ],
  (A, vMain) => {
    A.only(() => interact.checkView(0, MUInt.None()));
    A.only(() => interact.checkView(1, MUInt.None()));
    A.only(() => interact.checkView(2, MUInt.None()));

    A.publish();
    vMain.f.is((x) => x + 1);
    A.only(() => interact.checkView(0, MUInt.Some(1)));
    A.only(() => interact.checkView(1, MUInt.Some(2)));
    A.only(() => interact.checkView(2, MUInt.Some(3)));
    commit();

    A.publish();
    vMain.f.is((x) => x - 1);
    A.only(() => interact.checkView(0, MUInt.None()));
    A.only(() => interact.checkView(1, MUInt.Some(0)));
    A.only(() => interact.checkView(2, MUInt.Some(1)));
    commit();

    A.publish();
    commit();

    A.only(() => interact.checkView(0, MUInt.None()));
    A.only(() => interact.checkView(1, MUInt.None()));
    A.only(() => interact.checkView(2, MUInt.None()));

    exit();
  }
);
