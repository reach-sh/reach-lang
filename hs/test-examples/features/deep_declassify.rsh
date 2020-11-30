'reach 0.1';

const Common = {
  get: Fun([], Object({
    x: UInt,
    y: Object({ z: UInt }),
  })),
};

export const main =
  Reach.App(
    {  },
    [['Alice',
      { ...Common } ],
     ['Bob',
      { ...Common } ],
    ],
    (Alice, Bob) => {
      Alice.only(() => {
        const o = declassify(interact.get());
      });
      Alice.publish(o);
      commit();

      Bob.only(() => {
        const b = (o.x == 5 && o.y.z == 10);
      });
      Bob.publish(b);
      commit();

      exit();
    });
