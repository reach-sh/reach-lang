'reach 0.1';

const [ _, ALICE_WINS, BOB_WINS, TIMEOUT ] = makeEnum(3);

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

        const F = Data({fAlice: Null, fBob: Null});
        Alice.only(() => {
          const fAlice = () => ({
            when: declassify(interact.keepGoing()) });
          const fRes0 = fAlice();
          const fRes1 = Object.setIfUnset(fRes0, "msg", null);
          const fRes2 = Object.setIfUnset(fRes1, "when", true)
          const fRes3 = Object.setIfUnset(fRes2, "pay", 0);
          const fRes = fRes3;
          const fMsg = F.fAlice(fRes.msg);
          const fWhen = fRes.when;
          const fPay = fRes.pay; });
        Bob.only(() => {
          const fBob = () => ({
            when: declassify(interact.keepGoing()) });
          const fRes0 = fBob();
          const fRes1 = Object.setIfUnset(fRes0, "msg", null);
          const fRes2 = Object.setIfUnset(fRes1, "when", true)
          const fRes3 = Object.setIfUnset(fRes2, "pay", 0);
          const fRes = fRes3;
          const fMsg = F.fBob(fRes.msg);
          const fWhen = fRes.when;
          const fPay = fRes.pay; });

        Anybody.publish(fMsg, fPay).when(fWhen).pay(fPay)
          .timeout(deadline, () => {
            showOutcome(TIMEOUT)();
            Anybody.publish();
            keepGoing = false;
            continue; });
        require(fMsg.match({fAlice: (() => this == Alice),
                             fBob  : (() => this == Bob  ) }));
        switch ( fMsg ) {
          case fAlice: {
            assert(this == Alice);
            const fHadPay = false; // <-- we know this by inspecting the object statically
            require(fHadPay || fPay == 0);
            // <code that Alice wrote>
            each([Alice, Bob], () => {
              interact.roundWinnerWas(true); });
            [ keepGoing, as, bs ] = [ true, as + 1, bs ];
            continue;
            // </>
          }
          case fBob: {
            assert(this == Bob);
            const fHadPay = false; // <-- we know this by inspecting the object statically
            require(fHadPay || fPay == 0);
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
