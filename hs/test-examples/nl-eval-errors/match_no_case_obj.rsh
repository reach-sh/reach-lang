'reach 0.1';

const Common = {
  get: Fun([], Data({ None: Null, Some: UInt })),
  put: Fun([UInt], Null),
};

export const main =
  Reach.App(
    {  },
    [['Alice',
      { ...Common } ]
    ],
    (Alice) => {

      const mx = Maybe(UInt).Some(5);
      const i = mx.match(4);

      Alice.only(() => {
        interact.put(i);
      });

      exit();
    });
