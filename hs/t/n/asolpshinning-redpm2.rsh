'reach 0.1';
const MUInt = Maybe(UInt);
const f = (y) => {
  switch (y) {
    case Some: { return [0, [y, 1]]; }
    case None: { return [0]; }
  }
};
export const main = Reach.App(() => {
  const C = Participant('C', { y: MUInt });
  init();
  C.only(() => { const x = declassify(f(interact.y)); });
  C.publish(x);
  commit();
});
