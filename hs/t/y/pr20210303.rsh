'reach 0.1';
export const main = Reach.App(
  { },
  [ Participant('O', {
      ...hasRandom,
      ping: Fun([], Null),
    }),
  ],
  (O) => {
    const ping = () =>
      O.only(() => interact.ping());
    O.only(() => {
      const _x = interact.random();
    });
    ping();
    O.only(() => {
      const x = declassify(_x);
    });
    O.publish(x);
    commit();
  });
