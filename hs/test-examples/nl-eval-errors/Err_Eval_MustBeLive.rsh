'reach 0.1';

export const main =
  Reach.App(
    {},
    [['Alice', {}]],
    (Alice) => {
      const f = () => { exit(); };

      f();
      Alice.publish();
      commit();

    });
