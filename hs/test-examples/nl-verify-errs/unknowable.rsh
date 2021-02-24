'reach 0.1';

const Interface = {
  x: UInt,
  y: UInt,
};

export const main =
  Reach.App(
    {},
    [Participant('Alice', Interface), Participant('Bob', {})],
    (A, B) => {
      A.only(() => {
        const x = declassify(interact.x);
        const y = x + 2;
        const y1 = y - 1;
        const z = y1;
      });
      A.publish(z);
      commit();

      unknowable(B, A(x));
    }
  );
