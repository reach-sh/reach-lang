/*
  1. A & B Select their ship locations.
  2. Locations for each are made into commitments.
  3. A & B have X number of guesses out of Y grid selections.
  4. Winner is determined by whoever guesses the most correct ship locations.
  5. If a Draw occurs, the game starts over at (1).
*/

'reach 0.1';

const GRID_SIZE = 9;
const DEADLINE = 10;
const [ isOutcome, B_WINS, DRAW, A_WINS ] = makeEnum(3);

const winner = (countA, countB) => {
  if (countA > countB) {
    return A_WINS;
  } else if (countB > countA) {
    return B_WINS;
  } else {
    return DRAW;
  }
}

assert(winner(1, 0) == A_WINS);
assert(winner(0, 1) == B_WINS);
assert(winner(1, 1) == DRAW);

const player = {
  ...hasRandom,
  seeOutcome: Fun([UInt], Null),
  informTimeout: Fun([], Null),
  selectShips: Fun([], Array(UInt, GRID_SIZE)),
  guessShips: Fun([], Array(UInt, GRID_SIZE)),
};
const deployer = {
  ...player,
  wager: UInt,
};
const attacher = {
  ...player,
  acceptWager: Fun([UInt], Null)
};

export const main = Reach.App(
  {},
  [Participant('deployer', deployer), Participant('attacher', attacher)],
  (A, B) => {
    const informTimeout = () => {
      each([A, B], () => {
        interact.informTimeout();
      });
    };

    // A declassifies and submits wager
    A.only(() => {
      const wager = declassify(interact.wager);
    });
    A.publish(wager).pay(wager);
    commit();

    // B accepts wager given an amount of time to accept
    B.only(() => {
      interact.acceptWager(wager);
    });

    B.pay(wager).timeout(DEADLINE, () => closeTo(A, informTimeout));

    // -> ON DRAW LOOP STARTS HERE
    var [ loopCount, outcome ] = [ 0, DRAW ];
    invariant(balance() == 2 * wager && isOutcome(outcome));
    while (outcome == DRAW) {
      commit();

      // A selects locations for ships and stores it in contract private.
      A.only(() => {
        const _shipsA = interact.selectShips();
        const [_commitA, _saltA] = makeCommitment(interact, _shipsA);
        const commitA = declassify(_commitA);
      });
      A.publish(commitA).timeout(DEADLINE, () => closeTo(B, informTimeout));
      commit();
      // B should not know the location of A's ships
      unknowable(B, A(_shipsA, _saltA));

      // B selects locations for ships and stores them in contract public
      B.only(() => {
        const _shipsB = interact.selectShips();
        const [_commitB, _saltB] = makeCommitment(interact, _shipsB);
        const commitB = declassify(_commitB);
      });
      B.publish(commitB).timeout(DEADLINE, () => closeTo(A, informTimeout));
      commit();
      // A should not know the location of B's ships
      unknowable(A, B(_shipsB, _saltB));


      // A guesses B's ship locations
      A.only(() => {
        const guessesA = declassify(interact.guessShips());
      });
      A.publish(guessesA).timeout(DEADLINE, () => closeTo(B, informTimeout));
      commit();
      // B guesses A's ship locations
      B.only(() => {
        const guessesB = declassify(interact.guessShips());
      });
      B.publish(guessesB).timeout(DEADLINE, () => closeTo(A, informTimeout));
      commit();


      // // A decrypts and stores ships locations on contract public
      // A.only(() => {
      //   const [saltA, shipsA] = declassify([_saltA, _shipsA]);
      // });
      // A.publish(saltA, shipsA);
      // checkCommitment(commitA, saltA, shipsA);
      // commit();
      // // A decrypts and stores ships locations on contract public
      // B.only(() => {
      //   const [saltB, shipsB] = declassify([_saltB, _shipsB]);
      // });
      // B.publish(saltB, shipsB);
      // checkCommitment(commitB, saltB, shipsB);

      // /*
      //   While loop is to slow due to requiring publish per loop.
      //   I devnet it took about 2-3 minutes to run a loop of 9
      //   There would also be the issue of making a transaction per
      //   per loop, this would cost a fee and require too much input
      //   from the user.

      //   I also had an issue where when sent to Anybody.publish() would result
      //   in a race condition between A and B where the participant that did
      //   not publish logs an error on the front-end.

      //   This is why I replaced the loop with manual checks. this is much
      //   faster in comparison making only one transaction compared to 9
      // */

      /* Determine who made the most correct guesses */
      var [ x, countA, countB ] = [ 0, 0, 0 ];
      invariant(balance() == wager * 2);
      while(x < GRID_SIZE) {
        each([A, B], () => {
          interact.loadingResult(x);
        });
        commit();
        // B is always the first to pick up this task.
        // This was originally set to Anybody.publish()
        // but this caused A to Post bad request (400)
        // When A tried to pick up the task.
        B.publish();

        [ x, countA, countB ] = [
          x + 1,
          ieq(shipsB[x], guessesA[x]) ? countA + 1 : countA,
          ieq(shipsA[x], guessesB[x]) ? countB + 1 : countB
        ];

        continue;
      }

      const countA_0 = ieq(shipsB[0], guessesA[0]) ? 1 : 0;
      const countB_0 = ieq(shipsA[0], guessesB[0]) ? 1 : 0;

      const countA_1 = ieq(shipsB[1], guessesA[1]) ? 1 : 0;
      const countB_1 = ieq(shipsA[1], guessesB[1]) ? 1 : 0;

      const countA_2 = ieq(shipsB[2], guessesA[2]) ? 1 : 0;
      const countB_2 = ieq(shipsA[2], guessesB[2]) ? 1 : 0;

      const countA_3 = ieq(shipsB[3], guessesA[3]) ? 1 : 0;
      const countB_3 = ieq(shipsA[3], guessesB[3]) ? 1 : 0;

      const countA_4 = ieq(shipsB[4], guessesA[4]) ? 1 : 0;
      const countB_4 = ieq(shipsA[4], guessesB[4]) ? 1 : 0;

      const countA_5 = ieq(shipsB[5], guessesA[5]) ? 1 : 0;
      const countB_5 = ieq(shipsA[5], guessesB[5]) ? 1 : 0;

      const countA_6 = ieq(shipsB[6], guessesA[6]) ? 1 : 0;
      const countB_6 = ieq(shipsA[6], guessesB[6]) ? 1 : 0;

      const countA_7 = ieq(shipsB[7], guessesA[7]) ? 1 : 0;
      const countB_7 = ieq(shipsA[7], guessesB[7]) ? 1 : 0;

      const countA_8 = ieq(shipsB[8], guessesA[8]) ? 1 : 0;
      const countB_8 = ieq(shipsA[8], guessesB[8]) ? 1 : 0;

      const countA = countA_0 + countA_1 + countA_2 + countA_3 + countA_4 + countA_5 + countA_6 + countA_7 + countA_8;
      const countB = countB_0 + countB_1 + countB_2 + countB_3 + countB_4 + countB_5 + countB_6 + countB_7 + countB_8;

      const outcome_hold = winner(countA, countB);

      each([A, B], () => {
        interact.seeOutcome(outcome_hold);
      });

      [ loopCount, outcome ] = [
        loopCount + 1,
        winner(countA, countB)
      ];

      continue;
    }

    assert(outcome == A_WINS || outcome == B_WINS);
    transfer(2 * wager).to(outcome == A_WINS ? A : B);
    commit();

    exit();
  }
)