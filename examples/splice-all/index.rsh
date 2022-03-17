'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  init();
  A.publish();
  commit();

  /*
   * This is a "white box" test that knows about how the csplice3 function in
   * ALGO.hs works. It makes sure that each case works. I verified that the
   * code below tests each case by changing it to `op "err"` as I wrote each
   * one.
   *
   */
  const au = (x) => array(UInt, x);

  // Both: Nothing
  {
    A.only(() => { const a = [ au([0, 0, 0]) ]; });
    A.publish(a);
    check(Array.set(a[0], 1, 1) == au([0, 1, 0]));
    commit();
  }
  // Both: Just
  {
    A.only(() => { const a = au([0, 0, 0]); });
    A.publish(a);
    check(Array.set(a, 1, 1) == au([0, 1, 0]));
    commit();
  }
  // After: Just
  {
    A.only(() => { const a = au([0, 0]); });
    A.publish(a);
    check(Array.set(a, 0, 1) == au([1, 0]));
    commit();
  }
  // After: Nothing
  {
    A.only(() => { const a = [ au([0, 0]) ]; });
    A.publish(a);
    check(Array.set(a[0], 0, 1) == au([1, 0]));
    commit();
  }
  // Before: Nothing
  {
    A.only(() => { const a = [ au([0, 0]) ]; });
    A.publish(a);
    check(Array.set(a[0], 1, 1) == au([0, 1]));
    commit();
  }
  // Before: Just
  {
    A.only(() => { const a = au([0, 0]); });
    A.publish(a);
    check(Array.set(a, 1, 1) == au([0, 1]));
    commit();
  }
  // None: Just
  {
    A.only(() => { const a = au([0]); });
    A.publish(a);
    check(Array.set(a, 0, 1) == au([1]));
    commit();
  }
  // None: Nothing
  {
    A.only(() => { const a = [ au([0]) ]; });
    A.publish(a);
    check(Array.set(a[0], 0, 1) == au([1]));
    commit();
  }

  A.publish();
  commit();
  exit();
});
