'reach 0.1';

const MUInt = Maybe(UInt);
export const main = Reach.App(() => {
  const A = Participant('Alice', {});
  deploy();

  A.publish();
  const m = new Map(UInt);
  assert(m[A] == MUInt.None(), "m initially empty");
  commit();

  exit();
});
