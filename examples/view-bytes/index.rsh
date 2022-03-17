'reach 0.1';

const BS = Bytes(96);
const MA = Maybe(Address);
const MBS = Maybe(BS);

const Common = {
  checkView: Fun([Tuple(MA, MBS)], Null),
};
export const main = Reach.App(() => {
  const A = Participant('Alice', {
    ...Common,
    meta: BS,
  });
  const B = Participant('Bob', {
    ...Common,
  });
  const vMain = View('Main', {
    who: Address,
    meta: BS,
  });
  init();

  A.publish(); commit();
  A.interact.checkView([MA.None(), MBS.None()]);

  A.only(() => { const meta = declassify(interact.meta); });
  A.publish(meta);
  vMain.who.set(A);
  vMain.meta.set(meta);
  commit();
  A.interact.checkView([MA.Some(A), MBS.Some(meta)]);

  A.publish();
  vMain.who.set();
  commit();
  A.interact.checkView([MA.None(), MBS.Some(meta)]);

  A.publish();
  vMain.who.set(A);
  vMain.meta.set();
  commit();
  A.interact.checkView([MA.Some(A), MBS.None()]);

  A.publish(); commit();
  B.interact.checkView([MA.None(), MBS.None()]);

  exit();
});
