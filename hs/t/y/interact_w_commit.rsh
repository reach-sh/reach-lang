'reach 0.1';

const I = {
  // Can't use shadow stdlib functions within objects
  commit: Fun([], UInt)
};

export const main =
  Reach.App(
    {},
    [['A', I]],
    (A) => {
      A.only(() => {
        const x = declassify(interact.commit());
      });
      A.publish(x);
      commit();
      exit();
    });
