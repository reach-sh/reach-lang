'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      const mx = Maybe(UInt).None();
      const isNoneDef = (m) =>
        m.match({
          Some: (_) => { return false; },
          default: (_) => true });
      assert(isNoneDef(mx));
    });
