'reach 0.1';

export const main =
 Reach.App({},
  [ Participant('Alice', {}),
    Participant('Bob', {}),
    View('Main', { last: Address, i: UInt }),
  ],
  (A, B, vMain) => {
    // Step 1
    vMain.i.is(1);
    A.publish();
    vMain.last.is(A);
    commit();

    // Step 2
    vMain.i.is(2);
    B.publish();
    vMain.last.is(B);

    if ( A == B ) {
      commit();
      // Step 3
      vMain.last.is();
      vMain.i.is(3);
      A.publish();
    }

    commit();
    // Step 4
    vMain.i.is(4);

    exit();
  });
