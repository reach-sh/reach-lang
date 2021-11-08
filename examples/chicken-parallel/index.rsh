'reach 0.1';
'use strict';

const [ _, ALICE_WINS, BOB_WINS, TIMEOUT ] = makeEnum(3);

const Common = {
  showOutcome: Fun([UInt], Null),
  keepGoing: Fun([], Bool),
  roundWinnerWas: Fun([Bool], Null),
};

export const main =
  Reach.App(
    { },
    [Participant('Alice',
      { ...Common,
        getParams: Fun([], Object({ wager: UInt,
                                    deadline: UInt })) }),
     Participant('Bob',
      { ...Common,
        confirmWager: Fun([UInt], Null) } ),
    ],
    (Alice, Bob) => {
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

      const [ keepGoing, as, bs ] =
        parallelReduce([ true, 0, 0 ])
        .invariant(balance() == 2 * wager)
        .while(keepGoing)
        .case(Alice, (() => ({
          when: declassify(interact.keepGoing()) })),
          (_) => {
            each([Alice, Bob], () => {
              interact.roundWinnerWas(true); });
            return [ true, 1 + as, bs ]; })
        .case(Bob, (() => ({
          when: declassify(interact.keepGoing()) })),
          (_) => {
            each([Alice, Bob], () => {
              interact.roundWinnerWas(false); });
            return [ true, as, 1 + bs ]; })
        .timeout(relativeTime(deadline), () => {
          showOutcome(TIMEOUT)();
          Anybody.publish();
          return [ false, as, bs ]; });

      const outcome = bs > as ? BOB_WINS : ALICE_WINS;
      const winner = outcome == ALICE_WINS ? Alice : Bob;
      transfer(balance()).to(winner);
      commit();
      showOutcome(outcome)();
    });
