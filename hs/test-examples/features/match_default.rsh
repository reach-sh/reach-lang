'reach 0.1';

export const main =
  Reach.App(
    {},
    [['A', {}]],
    (A) => {
      const mx = Maybe(UInt).None();
      const isNone = (m) =>
        m.match({
          Some: (_) => { return false; },
          default: () => true,
        });
      assert(isNone(mx));
    });
