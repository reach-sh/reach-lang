'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('Alice', {}),
     Participant('Bob', {}),
     Participant('Claire', {}),
    ],
    (Alice, Bob, Claire) => {
      Alice.only(() => {
        const x = 1; });
      Bob.only(() => {
        const x = 2; });
      Claire.only(() => {
        const x = 3; });
      race(Alice, Bob).publish(x);
      commit();
    });

