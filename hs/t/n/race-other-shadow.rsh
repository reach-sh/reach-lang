'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('Alice', {}),
     Participant('Bob', {}),
     Participant('Claire', {}),
     Participant('Constructor', {}),
    ],
    (Alice, Bob, Claire, Constructor) => {
      Constructor.publish();
      commit();
      Alice.only(() => {
        const x = 1; });
      Bob.only(() => {
        const x = 2; });
      Claire.only(() => {
        const x = 3; });
      race(Alice, Bob).publish(x);
      commit();
    });

