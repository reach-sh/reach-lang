'reach 0.1';

const [ isOutcome, ALICE_WINS, BOB_WINS, TIMEOUT ] = makeEnum(3);

const Common = {
  showOutcome: Fun([UInt], Null),
  keepGoing: Fun([], Bool),
  roundWinnerWas: Fun([Bool], Null),
};

export const main =
  Reach.App(
    { 'deployMode': 'firstMsg' },
    [Participant('Alice',
      { ...Common,
        getParams: Fun([], Object({ wager: UInt,
                                    deadline: UInt })) }),
     Participant('Bob',
      { ...Common,
        confirmWager: Fun([UInt], Null) }),
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
        .timeout(deadline, () => closeTo(Alice, showOutcome(TIMEOUT)));

      var [ keepGoing, as, bs ] = [ true, 0, 0 ];
      invariant(balance() == 2 * wager);
      while ( keepGoing ) {
        commit();

        const F = Data({F_Alice: Null, F_Bob: Null});
        Alice.only(() => {
          const f_Alice = () => ({
            when: declassify(interact.keepGoing()) });
          const f_res0 = f_Alice();
          const f_res1 = Object.setIfUnset(f_res0, "msg", null);
          const f_res2 = Object.setIfUnset(f_res1, "when", true)
          const f_res3 = Object.setIfUnset(f_res2, "pay", 0);
          const f_res = f_res3;
          const f_msg = F.F_Alice(f_res.msg);
          const f_when = f_res.when;
          const f_pay = f_res.pay; });
        Bob.only(() => {
          const f_Bob = () => ({
            when: declassify(interact.keepGoing()) });
          const f_res0 = f_Bob();
          const f_res1 = Object.setIfUnset(f_res0, "msg", null);
          const f_res2 = Object.setIfUnset(f_res1, "when", true)
          const f_res3 = Object.setIfUnset(f_res2, "pay", 0);
          const f_res = f_res3;
          const f_msg = F.F_Bob(f_res.msg);
          const f_when = f_res.when;
          const f_pay = f_res.pay; });

        Anybody.publish(f_msg, f_pay).when(f_when).pay(f_pay)
          .timeout(deadline, () => {
            showOutcome(TIMEOUT)();
            Anybody.publish();
            keepGoing = false;
            continue; });
        require(f_msg.match({F_Alice: (() => this == Alice),
                             F_Bob  : (() => this == Bob  ) }));
        switch ( f_msg ) {
          case F_Alice: {
            assert(this == Alice);
            const f_hadpay = false; // <-- we know this by inspecting the object statically
            require(f_hadpay || f_pay == 0);
            // <code that Alice wrote>
            each([Alice, Bob], () => {
              interact.roundWinnerWas(true); });
            [ keepGoing, as, bs ] = [ true, as + 1, bs ];
            continue;
            // </>
          }
          case F_Bob: {
            assert(this == Bob);
            const f_hadpay = false; // <-- we know this by inspecting the object statically
            require(f_hadpay || f_pay == 0);
            // <code that Bob wrote>
            each([Alice, Bob], () => {
              interact.roundWinnerWas(false); });
            [ keepGoing, as, bs ] = [ true, as, bs + 1 ];
            continue;
            // </>
          }
        }
      }

      const outcome = bs > as ? BOB_WINS : ALICE_WINS;
      const winner = outcome == ALICE_WINS ? Alice : Bob;
      transfer(balance()).to(winner);
      commit();
      showOutcome(outcome)();
    });
