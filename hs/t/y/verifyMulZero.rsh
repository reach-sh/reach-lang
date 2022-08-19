'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    params: Object({
      x: UInt,
      y: UInt,
    }),
    show: Fun(true, Null),
  });
  setOptions({ verifyArithmetic: true });
  init();

  A.only(() => {
    const params = declassify(interact.params);
    check(params.x != 0);
    check(params.y <= UInt.max / params.x);
  });
  A.publish(params);
  check(params.x != 0);
  check(params.y <= UInt.max / params.x);

  const { x, y } = params;

  const r = x * y;
  // Just in case someone reverses the order in the compiler for some reason...
  const r2 = y * x;

  A.interact.show(r);
  A.interact.show(r2);
  commit();

});
