'reach 0.1';

const [ isOutcome, ALICE_WINS, BOB_WINS, TIMEOUT ] = makeEnum(3);

const Common = {
  showOutcome: Fun([UInt], Null),
  keepGoing: Fun([], Bool)
};

export const main =
  Reach.App(
    { 'deployMode': 'firstMsg' },
    [['Alice',
      { ...Common,
        getParams: Fun([], Object({ wager: UInt,
                                    deadline: UInt })) }],
     ['Bob',
      { ...Common,
        confirmWager: Fun([UInt], Null) } ],
    ],
    (Alice, Bob) => {
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

        const [ keepGoing_, as_, bs_ ] =
          parallel_reduce(
            deadline,
            [ false, as, bs ],
            invariant(balance() == 2 * wager),
            [
              [ Alice,
                () => {
                  Alice.only(() => {
                    if ( ! interact.keepGoing() ) {
                      fail(); } });
                  Alice.publish();
                  return [ true, 1 + as_, bs_ ]; } ],
              [ Bob,
                () => {
                  Bob.only(() => {
                    if ( ! interact.keepGoing() ) {
                      fail(); } });
                  Bob.publish();
                  return [ true, as_, 1 + bs_ ]; } ]
            ]);

        // This example is ugly because the syntax of continue and
        // parallel_reduce are not composable
        [ keepGoing, as, bs ] = [ keepGoing_, as_, bs_ ];
        continue;
      }

      const outcome = bs > as ? BOB_WINS : ALICE_WINS;
      const winner = outcome == ALICE_WINS ? Alice : Bob;
      transfer(balance()).to(winner);
      commit();
      showOutcome(outcome);
    });
