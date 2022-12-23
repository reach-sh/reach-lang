'reach 0.1';
export const main = Reach.App(() => {
  const A = Participant('A', {
    r: Contract,
  });
  init();
  A.only(() => {
    const r = declassify(interact.r);
  });
  A.publish(r);
  const ro = remote(r, { f: Fun([], UInt) });
  const _ = ro.f.ALGO({
    boxes: [ ["foo", "bar"] ],
  })();
  commit();
  exit();
});

