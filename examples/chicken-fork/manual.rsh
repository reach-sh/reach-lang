'reach 0.1';
'use strict';

const [ _, ALICE_WINS, BOB_WINS, TIMEOUT ] = makeEnum(3);

const Common = {
  showOutcome: Fun([UInt], Null),
  keepGoing: Fun([], Bool),
  roundWinnerWas: Fun([Bool], Null),
};

export const main = Reach.App(() => {
  setOptions({ 'deployMode': 'firstMsg' });
  const Alice = Participant('Alice', {
    ...Common,
    getParams: Fun([], Object({
      wager: UInt,
      deadline: UInt,
    }))
  });
  const Bob = Participant('Bob', {
    ...Common,
    confirmWager: Fun([UInt], Null),
  });
  deploy();

  const showOutcome = (which) => () => {
    each([Alice, Bob], () =>
      interact.showOutcome(which)); };

  Alice.only(() => {
    const { wager, deadline } =
      declassify(interact.getParams());
  });
  Alice.publish(wager, deadline)
    .pay(wager);
  commit();

  Bob.only(() => {
    interact.confirmWager(wager); });
  Bob.pay(wager)
    .timeout(deadline, () => closeTo(Alice, showOutcome(TIMEOUT)));

  var [ keepGoing, as, bs ] = [ true, 0, 0 ];
  invariant(balance() == 2 * wager);
  while ( keepGoing ) {
    commit();

    const D = Data({Alice: Null, Bob: Null});

    Alice.only(() => {
      const w = declassify(interact.keepGoing());
      const m = D.Alice();
    });
    Bob.only(() => {
      const w = declassify(interact.keepGoing());
      const m = D.Bob();
    });
    race(Alice, Bob)
      .publish(m)
      .when(w)
      .timeout(deadline, () => {
        showOutcome(TIMEOUT)();
        Anybody.publish();
        keepGoing = false;
        continue;
      });
    switch (m) {
      case Alice: {
        each([Alice, Bob], () => {
          interact.roundWinnerWas(true); });
        [ keepGoing, as, bs ] = [ true, as + 1, bs ];
        continue;
      }
      case Bob: {
        each([Alice, Bob], () => {
          interact.roundWinnerWas(false); });
        [ keepGoing, as, bs ] = [ true, as, bs + 1 ];
        continue;
      }
    };
  }

  const outcome = bs > as ? BOB_WINS : ALICE_WINS;
  const winner = outcome == ALICE_WINS ? Alice : Bob;
  transfer(balance()).to(winner);
  commit();
  showOutcome(outcome)();
});
