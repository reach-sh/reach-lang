'reach 0.1';

const BS = Bytes(96);
const MA = Maybe(Address);
const MBS = Maybe(BS);

export const main = Reach.App(
  {},
  [ Participant('Alice', {
      meta: BS,
    }),
    Participant('Bob', {
      checkView: Fun([Tuple(MA, MBS)], Null),
    }),
    View('Main', {
      who: Address,
      meta: BS,
    }),
  ],
  (A, B, vMain) => {
    A.publish(); commit();
    B.publish(); commit();
    B.interact.checkView([MA.None(), MBS.None()]);

    A.only(() => { const meta = declassify(interact.meta); });
    A.publish(meta);
    vMain.who.set(A);
    vMain.meta.set(meta);
    commit();
    B.publish(); commit();
    B.interact.checkView([MA.Some(A), MBS.Some(meta)]);

    A.publish();
    vMain.who.set();
    commit();
    B.publish(); commit();
    B.interact.checkView([MA.None(), MBS.Some(meta)]);

    A.publish();
    vMain.who.set(A);
    vMain.meta.set();
    commit();
    B.publish(); commit();
    B.interact.checkView([MA.Some(A), MBS.None()]);

    B.publish(); commit();
    A.publish(); commit();
    B.interact.checkView([MA.None(), MBS.None()]);

    exit();
  }
);
