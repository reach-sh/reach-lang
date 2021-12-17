'reach 0.1';
export const main = Reach.App(() => {
  const A = Participant('A', {});
  const P = API('P', { get: Fun([], UInt) });
  deploy();
  A.publish();
  const [x] = parallelReduce([0])
    .invariant(balance() == 0)
    .while(x < 5)
    .api(P.get, (apiReturn) => {
      const r = (() => {
        if (x < 1) {
          return 0;
        } else {
          var d = false;
          invariant(balance() == 0);
          while (!d) {
            commit();
            A.publish();
            d = true;
            continue;
          }
          return 0;
        }
      })();
      apiReturn(r);
      return [x];
    });
  commit();
  exit();
});
