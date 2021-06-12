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

      const f = (m) => m.match({
        None: 4,
        Some: (i) => { return i },
      });
      const i = f(Maybe(UInt).Some(5));
      const j = f(Maybe(UInt).None());

      Alice.only(() => {
        interact.put(i);
        interact.put(j);
      });

      exit();
    });
