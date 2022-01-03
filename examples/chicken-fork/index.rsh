'reach 0.1';
'use strict';

const [ _, ALICE_WINS, BOB_WINS, TIMEOUT ] = makeEnum(3);

const Common = {
  showOutcome: Fun([UInt], Null),
  keepGoing: Fun([], Bool),
  roundWinnerWas: Fun([Bool], Null),
};

export const main = Reach.App(() => {
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
  init();

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
    .timeout(relativeTime(deadline), () => closeTo(Alice, showOutcome(TIMEOUT)));

  var [ keepGoing, as, bs ] = [ true, 0, 0 ];
  invariant(balance() == 2 * wager);
  while ( keepGoing ) {
    commit();

    fork()
      .case(Alice, () => ({
        when: declassify(interact.keepGoing()) }),
        () => {
          each([Alice, Bob], () => {
            interact.roundWinnerWas(true); });
          [ keepGoing, as, bs ] = [ true, as + 1, bs ];
          continue; })
      .case(Bob, () => ({
        when: declassify(interact.keepGoing()) }),
        () => {
          each([Alice, Bob], () => {
            interact.roundWinnerWas(false); });
          [ keepGoing, as, bs ] = [ true, as, bs + 1 ];
          continue; })
      .timeout(relativeTime(deadline), () => {
        showOutcome(TIMEOUT)();
        Anybody.publish();
        keepGoing = false;
        continue; });
  }

  const outcome = bs > as ? BOB_WINS : ALICE_WINS;
  const winner = outcome == ALICE_WINS ? Alice : Bob;
  transfer(balance()).to(winner);
  commit();
  showOutcome(outcome)();
});
