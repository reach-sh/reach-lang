'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    params: Object({
      x: UInt,
      y: UInt,
      cx: UInt,
      cy: UInt,
    }),
    show: Fun(true, Null),
  });
  setOptions({ verifyArithmetic: true });
  deploy();

  A.only(() => {
    const params = declassify(interact.params);
    assume(params.cx > 0);
    assume(params.cy > 0);
    assume(params.cx <= UInt.max / params.cy);
    assume(params.x <= UInt.max);
    assume(params.y <= UInt.max);
    verifyMuldiv(params.x, params.y, params.cx * params.cy);
  });
  A.publish(params);

  const { x, y, cx, cy } = params;

  require(cx > 0);
  require(cy > 0);
  require(cx <= UInt.max / cy);
  require(x <= UInt.max);
  require(y <= UInt.max);
  verifyMuldiv(x, y, cx * cy);

  const j = muldiv(x, y, cx * cy);

  require(j <= UInt.max);
  A.interact.show(j);
  commit();
  verifyMuldiv(x, y, cx * cy);

});
