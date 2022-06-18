"reach 0.1";
export const main = Reach.App(() => {
  const D = Participant("D", {});
  const U = API("U", { f: Fun([UInt, UInt, UInt, UInt, UInt, UInt, UInt, UInt, UInt, UInt, UInt], Null) });
  init();
  D.publish();
  const x =
    parallelReduce(0)
    .invariant(balance() == 0)
    .while(true)
    .api_(U.f, (a, b, c, d, e, f, g, h, i, j, k) => {
      return [0, (ret) => {
        ret(null);
        return 0;
      }]});
  commit();
});
export const main3 = Reach.App(() => {
  const D = Participant("D", {});
  const U = API("U", { f: Fun([], Null) });
  init();
  D.publish();
  const x =
    parallelReduce(0)
    .invariant(balance() == 0)
    .while(true)
    .api_(U.f, () => {
      return [0, (ret) => {
        ret(null);
        return 0;
      }]});
  commit();
});
export const main4 = Reach.App(() => {
  const D = Participant("D", {});
  const U = API("U", { f: Fun([UInt], Null) });
  init();
  D.publish();
  const x =
    parallelReduce(0)
    .invariant(balance() == 0)
    .while(true)
    .api_(U.f, (a) => {
      return [0, (ret) => {
        ret(null);
        return 0;
      }]});
  commit();
});
export const main2 = Reach.App(() => {
  const D = Participant("D", {});
  const U = API("U", {
    f: Fun([UInt, UInt, UInt, UInt, UInt, UInt, UInt, UInt, UInt, UInt, UInt], Null),
    g: Fun([UInt, UInt, UInt, UInt, UInt, UInt, UInt, UInt, UInt, UInt, UInt], Null),
    h: Fun([], Null),
    i: Fun([UInt], Null),
  });
  init();
  D.publish();
  const x =
    parallelReduce(0)
    .invariant(balance() == 0)
    .while(true)
    .api_(U.f, (a, b, c, d, e, f, g, h, i, j, k) => {
      return [0, (ret) => {
        ret(null);
        return 0;
      }]})
    .api_(U.g, (a, b, c, d, e, f, g, h, i, j, k) => {
      return [0, (ret) => {
        ret(null);
        return 0;
      }]})
    .api_(U.h, () => {
      return [0, (ret) => {
        ret(null);
        return 0;
      }]})
    .api_(U.i, (a) => {
      return [0, (ret) => {
        ret(null);
        return 0;
      }]});
  commit();
});
