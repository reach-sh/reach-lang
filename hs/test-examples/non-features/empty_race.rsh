'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('Alice', { })],
    (Alice) => {
      Alice.pay(1);
      commit();

      race().publish();
      transfer(1).to(Alice);
      commit();
    });
