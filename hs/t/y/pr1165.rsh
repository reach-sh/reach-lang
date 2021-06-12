'reach 0.1';
export const main =
  Reach.App(
    {},
    [
      Participant('A', {
        i: UInt
      })
    ],
    (A) => {
      A.only(() => {
        const i = declassify(interact.i);
      })
      A.publish(i);
      if (i < 5) {
        commit();
      } else {
        commit();
      }
      exit();
    });
