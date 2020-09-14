'reach 0.1';

const MUInt256 = Maybe(UInt256);

export const main = Reach.App(
  {},
  [['A', { get1: Fun([], MUInt256),
           put1: Fun([MUInt256], Null) }]],
  (A) => {
    A.only(() => {
      const mi = declassify(interact.get1());
      const i = (() => {
        switch (mi) {
        case None: return 42;
        default: return mi+1; } })(); });
    A.publish(i);
    commit();

    exit(); });
