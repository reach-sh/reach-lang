'reach 0.1';

const DELAY = 10;
const Callback = Fun([], Null);
const Common = {
  got: Fun([], UInt)
};

export const main =
  Reach.App(
    {},
    [
      ['A', {
        ...Common,
        getA: Fun([], Tuple([UInt, UInt]))
      }],
      ['B', {
        ...Common,
        getB: Fun([Callback], Tuple([UInt, UInt]))
      }],
    ],
    (A, B) => {

      // This tuple is the thing returned by the race branches, which all
      // have to be the same type (potentially using Data to deal with the
      // different values)
      const [ isA, winner, x, cy] =
        race([
          [A,
            () => {
              const [ _xa, _ya ] = interact.getA();
              const xa = declassify(_xa);
              const cya = declassify(digest(_ya));
              publish(xa, cya)
                .pay(xa)
                // This will be run if A doesn't get selected
                .fail(() => {
                  interact.failed();
                });
              // This `me()` is the current participant
              return [ true, me(), xa, cya ];
            }],
          [B,
            () => {
              // B doesn't have a fail on his publish, so instead he passes a
              // fail to his interact, which allows him to escape from this
              // race if he doesn't want to run it.
              const [ _xb, _yb ] = interact.getB(fail);
              const xb = declassify(_xb);
              const cyb = declassify(digest(_yb));
              publish(xb, cyb)
                .pay(xb);
              return [ false, me(), xb, cyb ];
            }],
        ]);
      invariant(balance() == x);
      (isA ? A : B).set(winner);
      commit();

      const [ y ] =
        race([
          // Here we use the feature that multiple participants can have the
          // same race body
          [[A, B],
            () => {
              // This might be a bit controversial... the variables bound in
              // their earlier race bodies cannot be got via any method, but
              // they COULD be sent back to the frontend and requested again.
              // The problem, of course, is establishing that they are equal to
              // something earlier.
              const y = declassify(interact.got());
              assume(cy == digest(y));
              publish(y);
              return [ y ];
            }],
        ])
        // There's a timeout option on races of something else to happen if
        // this race doesn't get hit by anyone.
        .timeout(DELAY, () => {
          // XXX
        });
      invariant(balance() == x && cy == digest(y));
      require(cy == digest(y));
      transfer(balance()).to(winner);
      commit();
    });
