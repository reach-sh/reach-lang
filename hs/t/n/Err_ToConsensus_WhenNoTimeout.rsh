'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('Alice', { shouldPublish: Fun([], Bool) })],
    (Alice) => {
      Alice.pay(1);
      commit();

      Alice.only(() => {
        const go = declassify(interact.shouldPublish()); });
      Alice.publish().when(go);
      transfer(1).to(Alice);
      commit();
    });
