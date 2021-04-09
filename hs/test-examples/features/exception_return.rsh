'reach 0.1';

const returnsFromCatch = () => {
  try {
    throw 5;
    return false;
  } catch (e) {
    return true;
  }
}

const returnsFromTry = () => {
  try {
    return true;
  } catch (e) {
    return false;
  }
}

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      assert(returnsFromCatch());
      assert(returnsFromTry());
    });
