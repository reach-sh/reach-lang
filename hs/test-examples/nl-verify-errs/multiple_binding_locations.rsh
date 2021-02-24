'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', { x : UInt })],
    (A) => {
      A.only(() => {
        // Lets say `declassify(interact.x) - 4` evaluates
        // to DLVar with counter 4.
        const u = declassify(interact.x) - 4;
        const w = u;
        const x = u;
        const y = u;
        // Ensure that the SMT error displays `x` and not
        // another var with the same value: DLVar _ _ _ 4
        const z = x + x;
      });
      A.publish(y, z);
      commit();

      assert(z == 4);

    });
