'reach 0.1';

const MUInt256 = Maybe(UInt256);

export const main = Reach.App(
  {},
  [['A', { }]],
  (A) => {
    A.only(() => {
      const mi = MUInt256.None();
      const i = (() => {
        switch (mi) {
        case None: return 42;
        default: return mi+1; } })(); });
    A.publish(i);
    commit();

    exit(); });
