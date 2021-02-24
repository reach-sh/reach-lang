'reach 0.1';

const Common = {
  get: Fun([], Data({ None: Null, Some: UInt })),
  put: Fun([UInt], Null),
};

export const main =
  Reach.App(
    {  },
    [Participant('Alice',
      { ...Common })
    ],
    (Alice) => {

      const mx = Maybe(UInt).Some(5);
      const i = mx.match({
        None: 4,
        Some: (i) => { return i },
      });

      Alice.only(() => {
        interact.put(i);
      });

      exit();
    });
