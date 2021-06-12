'reach 0.1';
const XFixedPoint = Object({ scale: UInt, i: UInt });
const XmkFixedPoint = (scale) => (i) =>
  ({ scale, i });
const XfixedPointSqrt = (x, k) => {
  return { i : sqrt(x.i, k), scale: x.scale / sqrt(x.scale, k) };
}
export const main = Reach.App(
  {
    verifyPerConnector: true,
  },
  [Participant(
    'Alice',
    {
      show: Fun([XFixedPoint], Null)
    }
  )],
  (Alice) => {
    const fp100 = XmkFixedPoint(100);
    Alice.only(() => {
      const fp34_56 = fp100(3456);
      interact.show(XfixedPointSqrt(fp34_56, 10));
    });
  }
  );
