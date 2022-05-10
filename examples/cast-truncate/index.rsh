'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    x: UInt256,
    y: UInt256,
    truncX: Fun([UInt], Null),
    truncY: Fun([UInt], Null),
  });
  init();
  A.only(() => {
    const x = declassify(interact.x);
    const y = declassify(interact.y);
  })
  A.publish(x, y);
  A.interact.truncX(UInt(x, true));
  A.interact.truncY(UInt(y, true));
  commit();
  exit();
});
