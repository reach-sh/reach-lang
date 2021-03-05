'reach 0.1';

const Common = {
  showOpponent: Fun([Address], Null),
  keepGoing: Fun([], Bool),
  getParams: Fun([], Object({ wager: UInt, deadline: UInt })),
};

export const main =
  Reach.App(
    { 'deployMode': 'firstMsg' },
    [Participant('Alice', Common),
     Participant('Bob', Common),
    ],
    (Alice, Bob) => {
      Alice.only(() => {
        const { wager, deadline } =
          declassify(interact.getParams());
      });
      Alice.publish(wager, deadline)
        .pay(wager);
      commit();

      Bob.only(() => interact.showOpponent(Alice));

      fork()
      .case(Alice, (() => ({
        msg: 19,
        when: declassify(interact.keepGoing()) })),
        ((v) => v),
        (v) => {
          require(v == 19);
          transfer(wager + 19).to(this);
          commit();
          exit();
        })
      .case(Bob, (() => ({
        when: declassify(interact.keepGoing()) })),
        (() => wager),
        () => {
          commit();

          Alice.only(() => interact.showOpponent(Bob));

          Anybody.publish();
          transfer(2 * wager).to(this);
          commit();
          exit();
        })
      .timeout(deadline, () => {
        Anybody.publish();
        transfer(wager).to(this);
        commit();
        exit(); });
    });
