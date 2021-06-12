'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', { i: UInt })],
    (A) => {
      A.only(() => {
        const i = declassify(interact.i);
      });
      A.publish(i);

      try {
        if (i < 5) {
          throw true;
        } else {
          throw 5;
        }
      } catch (_) { }
    });
