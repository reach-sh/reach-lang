'reach 0.1';

export const main =
  Reach.App(
    {  },
    [['Alice', {}], ['Bob', {}],
    ],
    (Alice, Bob) => {
      closeTo(Bob,
        each([Alice, Bob], () => {
          interact.showResult(5); }));
    });
