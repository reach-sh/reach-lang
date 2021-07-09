'reach 0.1';

export const main = Reach.App(() => {
  const Alice = Participant('Alice', {
    show: Fun([UInt], Null),
  });
  deploy();

  Alice.publish();

  const [x, y] =
    parallelReduce([ 0, 1])
      .define(() => {
        const sum = () => x + y;
      })
      .invariant(balance() == 0 && sum() == sum())
      .while(sum() < 10)
      .case(
        Alice,
        (() => ({ when: sum() < 15 })),
        (() => {
          Alice.interact.show(x);
          Alice.interact.show(y);
          Alice.interact.show(sum());
          return [ x + 1, y + 1 ];
        })
      )
      .timeout(10, () => {
        Anybody.publish();
        Alice.interact.show(sum());
        return [ x, y ];
      });

  commit();
});
