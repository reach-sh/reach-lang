'reach 0.1';
'use strict';

const Player = {
  ask: Fun([], UInt),
};

const DELAY = 10;

// Stupid game: send money or do a thing if you don't.
function go(who, k) {
  who.only(() => {
    const amt = declassify(interact.ask());
  });
  who.publish(amt)
    .pay(amt)
    .timeout(relativeTime(DELAY), () => k());
  commit();
}

function thrice(f, x) {
  return f(f(f(x)));
}

export const main = Reach.App(
  {},
  [Participant('A', Player)],
  (A) => {
    A.publish();
    commit();

    const goThunk = (k) => () => go(A, k);
    const wrapUp = () => closeTo(A, () => {});
    const k3 = thrice(goThunk, wrapUp);

    /* This code makes some weird timeouts. The goal is so that
       msg1 = (-inf, +inf)
       \Y msg2 = [0, 10)
          \- msg3 = (-inf, +inf)
       \T msg4 = [10, 20)
          \Y msg5 = (-inf, +inf)
          \T msg6 = [20, 30)
             \Y msg7 = (-inf, +inf)
             \T msg8 = [30, +inf)
     */

    k3();
    wrapUp();
  }
);
