'reach 0.1';

export const main =
  Reach.App(
    {  },
    [Participant('Alice', {
        get: Fun([], Data({
          ENull: Null,
          EInt: UInt,
          EObj: Object({ i: UInt }) })),
        put: Fun([UInt], Null),
      })
    ],
    (Alice) => {
      Alice.only(() => {
        const mx = declassify(interact.get());
        const i = mx.match({
          ENull: () => 0,
          EInt: (x) => x,
          EObj: (o) => o.i,
        });
      });
      Alice.publish(i);
      commit();

      Alice.only(() => {
        interact.put(i);
      });
      exit();
    });
