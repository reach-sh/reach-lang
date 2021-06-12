'reach 0.1';

const MUInt = Maybe(UInt);

export const main = Reach.App(
  {},
  [Participant('A', { get1: Fun([], MUInt),
           put1: Fun([MUInt], Null) })],
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
