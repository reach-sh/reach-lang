'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('Alice', {})],
    (Alice) => {
      const f = () => { exit(); };

      f();
      Alice.publish();
      commit();

    });
