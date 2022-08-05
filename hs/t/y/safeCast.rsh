'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  init();
  const x  = UInt256.max;
  // Do not truncate and only verify at runtime
  const x_ = UInt(x, false, false);
});
