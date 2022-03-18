'reach 0.1';

const Tlast = Maybe(Address);
const Ti = Maybe(UInt);
const T = Tuple(Tlast, Ti);
const Common = { checkView: Fun([T], Null) };

export const main = Reach.App(() => {
  const A = Participant('Alice', Common);
  const B = Participant('Bob', Common);
  const vMain = View('Main', { last: Address, i: UInt });
  init();

  A.publish(); commit();

  // The contract doesn't exist yet, so no view
  A.interact.checkView([Tlast.None(), Ti.None()]);

  A.publish();
  vMain.i.set(1);
  vMain.last.set(A);
  // These views are now visible
  A.interact.checkView([Tlast.Some(A), Ti.Some(1)]);
  commit();

  // Block race of Alice and Bob for Alice to observe the state
  A.publish();
  commit();

  B.publish();
  vMain.i.set(2);
  vMain.last.set(B);
  if ( A != B ) {
    // The views above are visible
    A.interact.checkView([Tlast.Some(B), Ti.Some(2)]);
    commit();
  } else {
    // Or, we overwrite them
    vMain.i.set(3);
    vMain.last.set();
    A.interact.checkView([Tlast.None(), Ti.Some(3)]);
    commit();
  }

  A.publish();
  commit();
  // The contract doesn't exist anymore, so no view
  B.interact.checkView([Tlast.None(), Ti.None()]);

  exit();
});
