'reach 0.1';
export const main = Reach.App(() => {
  const A = Participant('A', {});
  const P = API('P', {
    f: Fun([], Bool),
    g: Fun([UInt], Bool),
    h: Fun([], Bool),
  });
  init();
  A.publish();
  const m = new Map(UInt, UInt);
  var [] = [];
  invariant(true);
  while (true) {
    commit();
    try {
      fork()
      .api_(P.g, (x) => {
        check(x != 0);
        return [ 0, (k) => {
          k(true);
          m[0] = x;
        } ];
      })
      .api_(P.h, () => {
        return [ 0, (k) => {
          k(true);
          commit();
          throw 0;
        } ];
      })
      .throwTimeout(relativeTime(1), 0);
      continue;
    } catch (e) {
      const [[], k] = call(P.f);
      k(true);
      transfer(fromSome(m[0], 0) * ((e > 0) ? 1 : 0)).to(this);
      continue;
    }
  }
  commit();
  exit();
});
