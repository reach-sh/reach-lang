'reach 0.1';

const DELAY = 10;
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
        getB: Fun([], Tuple(Maybe([UInt, UInt])))
      }],
    ],
    (A, B) => {

      // This tuple is the thing returned by the race branches, which all
      // have to be the same type (potentially using Data to deal with the
      // different values)
      const [ winner, x, cy] =
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
              return [ me(), xa, cya ];
            }],
          [B,
            () => {
              // B doesn't have a fail on his publish, so instead, he calls
              // fail
              const mg = interact.getB();
              switch ( mg ) {
                case None: fail();
                case Some:
                  const [ _xb, _yb ] = mg;
                  const xb = declassify(_xb);
                  const cyb = declassify(digest(_yb));
                  publish(xb, cyb)
                    .pay(xb);
                  return [ me(), xb, cyb ];
              }
            }],
        ]);
      // If there's a unique continuation for each race body, then this would
      // not be necessary. Any "global variables" like that would be effected.
      // The one that I don't know how to deal with are the participant
      // identities. If A wins the race, how do we know that future references
      // to A resolve to the same address? In terms of the code, A.set(addr),
      // couldn't be run because we don't know which participant it is (by
      // definition)
      //
      // A unique continuation would mean copying the code for each one, while
      // the non-unique continuation would not have that problem. This would
      // mean the code explosion would be exponential in the number of race
      // participants, which is bad. We already do exactly this with `if`
      // statements generally, so it might not be so bad. We could provide that
      // feature via something that you would put in an `if` condition, like
      // `if ( race() ) {} else {}`
      //
      // Another solution is to force races to involve only participants that
      // are already bound.
      invariant(balance() == x);
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
          // XXX The aforementioned problem with participant identities means
          // that this timeout sender is hard to specify, although maybe it is
          // simpler to have a "bystander" come in that could be the other one
        });
      invariant(balance() == x && cy == digest(y));
      require(cy == digest(y));
      transfer(balance()).to(winner);
      commit();

      // XXX We could provide something like `race` for classes that specified
      // a "combining" function that took the return values and combined them
      // together, as well as a deadline. This would not have the "participant
      // identity" problem above.
    });
