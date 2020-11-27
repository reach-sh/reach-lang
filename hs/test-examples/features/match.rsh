'reach 0.1';

export const main =
  Reach.App(
    {  },
    [['Alice', {
        get: Fun([], Data({
          ENull: Null,
          EInt: UInt,
          EObj: Object({ i: UInt }) })),
        put: Fun([UInt], Null),
      },
    ]],
    (Alice) => {
      Alice.only(() => {
        const mx = declassify(interact.get());
        const i = mx.match({
          ENull: () => { return 0; },
          EInt: (x) => { return x; },
          EObj: (o) => { return o.i; },
        });
      });
      Alice.publish(i);
      commit();

      Alice.only(() => {
        interact.put(i);
      });
      exit();
    });
