'reach 0.1';

const Tlast = Maybe(Address);
const Ti = Maybe(UInt);
const T = Tuple(Tlast, Ti);

export const main =
 Reach.App({},
  [ Participant('Alice', { checkView: Fun([T], Null) }),
    Participant('Bob', {}),
    View('Main', { last: Address, i: UInt }),
  ],
  (A, B, vMain) => {
    const checkView = (x) =>
      A.only(() => interact.checkView(x));

    // The contract doesn't exist yet, so no view
    checkView([Tlast.None(), Ti.None()]);

    A.publish();
    vMain.i.set(1);
    vMain.last.set(A);
    // These views are now visible
    checkView([Tlast.Some(A), Ti.Some(1)]);
    commit();

    // Block race of Alice and Bob for Alice to observe the state
    A.publish();
    commit();

    B.publish();
    vMain.i.set(2);
    vMain.last.set(B);
    if ( A != B ) {
      // The views above are visible
      checkView([Tlast.Some(B), Ti.Some(2)]);
      commit();
    } else {
      // Or, we overwrite them
      vMain.i.set(3);
      vMain.last.set();
      checkView([Tlast.None(), Ti.Some(3)]);
      commit();
    }

    A.publish();
    // The contract doesn't exist anymore, so no view
    checkView([Tlast.None(), Ti.None()]);
    commit();

    exit();
  });
