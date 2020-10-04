'reach 0.1';

const Player = {
  ask: Fun([], UInt256),
};

const DELAY = 10;

// Stupid game: send money or do a thing if you don't.
function go(who, kOk, kTime) {
  who.only(() => {
    const amt = declassify(interact.ask());
  });
  who.publish(amt)
    .pay(amt)
    .timeout(DELAY, () => kTime());
  commit();
  kOk();
}

function thrice(f, x, y) {
  return f(x, f(x, f(x, y)));
}

export const main = Reach.App(
  {},
  [['A', Player]],
  (A) => {
    A.publish();
    commit();

    const goThunk = (kOk, kTime) => () => go(A, kOk, kTime);
    const wrapUp = () => closeTo(A, () => {});
    const k3 = thrice(goThunk, wrapUp, wrapUp);

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
  }
);
