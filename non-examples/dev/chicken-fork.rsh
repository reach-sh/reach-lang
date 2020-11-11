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
      const showOutcome = (which) => {
        each([Alice, Bob], () => { interact.showOutcome(which); }); };

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
        .timeout(deadline, () => closeTo(Alice, () => showOutcome(TIMEOUT)));

      var [ keepGoing, as, bs ] = [ true, 0, 0 ];
      invariant(balance() == 2 * wager);
      while ( keepGoing ) {
        commit();
        fork([
          [ Alice,
            () => {
              Alice.only(() => {
                if ( ! interact.keepGoing() ) {
                  fail(); } });
              Alice.publish();
              [ keepGoing, as, bs ] = [ true, 1 + as, bs ];
              continue; } ],
          [ Bob,
            () => {
              Bob.only(() => {
                if ( ! interact.keepGoing() ) {
                  fail(); } });
              Bob.publish();
              [ keepGoing, as, bs ] = [ true, as, 1 + bs ];
              continue; } ]
        ])
        .timeout(deadline, () => {
          [ keepGoing, as, bs ] = [ false, as, bs ];
          continue; });
      }

      const outcome = bs > as ? BOB_WINS : ALICE_WINS;
      const winner = outcome == ALICE_WINS ? Alice : Bob;
      transfer(balance()).to(winner);
      commit();
      showOutcome(outcome);
    });
