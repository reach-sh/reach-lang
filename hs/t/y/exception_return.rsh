'reach 0.1';

const returnsFromCatch = () => {
  try {
    throw 5;
  } catch (e) {
    return true;
  }
}

const returnsFromTry = (x) => {
  try {
    if (x < x) {
      throw false;
    }
    return true;
  } catch (e) {
    return false;
  }
}

export const main =
  Reach.App(
    {},
    [Participant('A', { x: UInt })],
    (A) => {
      A.only(() => {
        const x = declassify(interact.x);
      });
      A.publish(x);

      assert(returnsFromCatch());
      assert(returnsFromTry(x), "return from try");
      commit();
    });
