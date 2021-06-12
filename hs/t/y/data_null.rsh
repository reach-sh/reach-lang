'reach 0.1';

const MUInt = Maybe(UInt);

export const main = Reach.App(
  {},
  [Participant('A', { })],
  (A) => {
    A.only(() => {
      const mi = MUInt.None();
      const i = (() => {
        switch (mi) {
        case None: return 42;
        default: return mi+1; } })(); });
    A.publish(i);
    commit();

    exit(); });
